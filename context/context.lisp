;;;; /context — display context window usage in the message window
;;;; Faithful port of @oh-my-pi/pi-coding-agent's /context command.
;;;; See: src/slash-commands/helpers/context-report.ts
;;;;       src/modes/utils/context-usage.ts

;;; ---------------------------------------------------------------------------
;;; Token estimation (same heuristic as oh-my-pi: chars/4)
;;; ---------------------------------------------------------------------------

(defun estimate-tokens (content)
  "Rough token count: ceiling of char-length / 4."
  (etypecase content
    (null 0)
    (string (ceiling (length content) 4))
    (list (reduce #'+ content :key #'estimate-tokens))))

(defun estimate-message-tokens (message)
  "Estimate tokens for a single session-log message."
  (estimate-tokens (kli/session/log:message-content message)))

;;; ---------------------------------------------------------------------------
;;; ASCII bar (from format.ts: renderAsciiBar)
;;; ---------------------------------------------------------------------------

(defun render-ascii-bar (fraction &key (width 24))
  "Render a [█████░░░░] 42% bar.  FRACTION is 0.0–1.0."
  (let* ((clamped (min (max (or fraction 0.0) 0.0) 1.0))
         (filled (round (* clamped width)))
         (empty  (max 0 (- width filled)))
         (pct    (round (* clamped 100))))
    (format nil "[~A~A] ~D%"
            (make-string filled :initial-element #\█)
            (make-string empty  :initial-element #\░)
            pct)))

;;; ---------------------------------------------------------------------------
;;; Human-friendly token counts (from @oh-my-pi/pi-utils: formatNumber)
;;; ---------------------------------------------------------------------------

(defun humanize-tokens (n)
  "Compact token count: bare integer below 1k, else N.Nk."
  (cond ((null n) "?")
        ((< n 1000) (format nil "~D" n))
        (t (let ((k (/ n 1000.0)))
             (if (= k (ffloor k))
                 (format nil "~Dk" (truncate k))
                 (format nil "~,1Fk" k))))))

;;; ---------------------------------------------------------------------------
;;; Category breakdown (from context-usage.ts: computeContextBreakdown)
;;; ---------------------------------------------------------------------------

(defun compute-context-breakdown (context)
  "Return a plist of breakdown data mimicking oh-my-pi's ContextBreakdown."
  (let* ((registry (kli:context-registry context))
         (service (or (kli:find-live-object registry :agent-session-service)
                       (error "No agent-session service is loaded.")))
         (mode-id :default-mode)
         (info (kli/agent/session:session-mode-info service mode-id context))
         (usage (and info (getf info :usage)))
         ;; Model registry & context window
         (model-registry (or (kli:find-live-object registry :model-registry-service)
                              (error "No model registry is loaded.")))
         (selection (kli/model/registry:current-model-selection model-registry))
         (context-window (kli/model/registry:selection-context-window
                          model-registry selection))
         ;; Projected messages from the agent-context
         (agent-context (kli/agent/session:agent-session-context
                         service mode-id context))
         (messages (when agent-context
                     (kli/context/lens:context-projected-messages agent-context)))
         ;; Partition messages by role
         (conversation-msgs (remove-if-not
                             (lambda (m) (member (kli/session/log:message-role m)
                                                 '(:user :assistant)))
                             messages))
         (tool-result-msgs (remove-if-not
                            (lambda (m) (eq (kli/session/log:message-role m) :tool-result))
                            messages))
         ;; Token estimation per category
         (conversation-tokens (reduce #'+ conversation-msgs :key #'estimate-message-tokens))
         (tool-output-tokens (reduce #'+ tool-result-msgs :key #'estimate-message-tokens))
         ;; Total reported usage
         (total-reported (and usage (kli/agent/session:usage-total-tokens usage)))
         (input-tokens  (and usage (kli/agent/session:usage-input-tokens usage)))
         (output-tokens (and usage (kli/agent/session:usage-output-tokens usage)))
         (cache-read    (and usage (kli/agent/session:usage-cache-read-tokens usage)))
         (cache-write   (and usage (kli/agent/session:usage-cache-write-tokens usage)))
         ;; Trailing estimate (unreported in-flight text)
         (trailing (kli/tui/status:trailing-token-estimate messages))
         ;; System prompt tokens = input - conversation - tool-output
         ;; (system prompt, tools schema, skills are all in the input)
         (system-tokens (max 0 (- (or input-tokens 0)
                                  conversation-tokens
                                  tool-output-tokens)))
         (estimated-total (+ (or total-reported 0) (or trailing 0)))
         ;; Compaction validity
         (store (kli:find-live-object registry :session-store))
         (session-id (and info (getf info :id)))
         (session (and store session-id
                       (kli/session/log:find-session store session-id)))
         (entries (and store session
                       (kli/session/log:session-branch store session nil)))
         (usage-known-p (not (kli/tui/status:usage-unknown-after-compaction-p
                              (or entries '()))))
         ;; Auto-compact buffer
         (compaction-policy (kli/agent/session:session-compaction-policy service))
         (compaction-enabled-p (funcall compaction-policy :enabled))
         (threshold-ratio (or (funcall compaction-policy :threshold-ratio) 0.85))
         (auto-compact-buffer
           (if (and context-window (plusp context-window) compaction-enabled-p)
               (let ((threshold (floor (* threshold-ratio context-window))))
                 (max 0 (- context-window threshold)))
               0))
         (auto-compact-buffer
           (min auto-compact-buffer
                (max 0 (- (or context-window 0) estimated-total))))
         (free-tokens
           (max 0 (- (or context-window 0) estimated-total auto-compact-buffer)))
         ;; Model info
         (provider (and info (getf info :provider)))
         (model    (and info (getf info :model)))
         (thinking (and info (getf info :thinking))))
    (list :context-window context-window
          :provider provider
          :model model
          :thinking thinking
          :system-tokens system-tokens
          :conversation-tokens conversation-tokens
          :tool-output-tokens tool-output-tokens
          :input-tokens input-tokens
          :output-tokens output-tokens
          :cache-read cache-read
          :cache-write cache-write
          :total-reported total-reported
          :trailing trailing
          :estimated-total estimated-total
          :usage-known-p usage-known-p
          :auto-compact-buffer auto-compact-buffer
          :compaction-enabled-p compaction-enabled-p
          :free-tokens free-tokens
          :messages-count (length messages))))

;;; ---------------------------------------------------------------------------
;;; Report rendering (from context-report.ts: buildContextReportText)
;;; ---------------------------------------------------------------------------

(defun context-report (context)
  "Build the /context text report, matching oh-my-pi's ACP-mode layout."
  (let ((b (compute-context-breakdown context)))
    (let ((context-window (getf b :context-window))
          (estimated-total (getf b :estimated-total))
          (usage-known-p (getf b :usage-known-p))
          (system-tokens (getf b :system-tokens))
          (conversation-tokens (getf b :conversation-tokens))
          (tool-output-tokens (getf b :tool-output-tokens))
          (auto-compact-buffer (getf b :auto-compact-buffer))
          (free-tokens (getf b :free-tokens))
          (compaction-enabled-p (getf b :compaction-enabled-p))
          (total-reported (getf b :total-reported))
          (trailing (getf b :trailing))
          (input-tokens (getf b :input-tokens))
          (output-tokens (getf b :output-tokens))
          (cache-read (getf b :cache-read))
          (cache-write (getf b :cache-write)))
      (when (or (null context-window) (<= context-window 0))
        (return-from context-report
          "Context usage is unavailable: no model is selected for this session."))
      (let* ((used-pct (if usage-known-p
                           (round (* 100 (/ estimated-total context-window)))
                           nil))
             (lines
               (list
                (format nil "Context window: ~A tokens (~D% used)"
                        (humanize-tokens context-window)
                        (or used-pct 0)))))
        ;; Category bars (skip zero-token categories)
        (when (plusp system-tokens)
          (push (format nil "  ~A  ~A  ~A tokens"
                         "System prompt    "
                         (render-ascii-bar (/ system-tokens context-window))
                         (humanize-tokens system-tokens))
                lines))
        (when (plusp conversation-tokens)
          (push (format nil "  ~A  ~A  ~A tokens"
                         "Conversation     "
                         (render-ascii-bar (/ conversation-tokens context-window))
                         (humanize-tokens conversation-tokens))
                lines))
        (when (plusp tool-output-tokens)
          (push (format nil "  ~A  ~A  ~A tokens"
                         "Tool output      "
                         (render-ascii-bar (/ tool-output-tokens context-window))
                         (humanize-tokens tool-output-tokens))
                lines))
        ;; Auto-compact buffer
        (when (and compaction-enabled-p (plusp auto-compact-buffer))
          (push (format nil "  ~A  ~A  ~A tokens"
                         "Auto-compact buf "
                         (render-ascii-bar (/ auto-compact-buffer context-window))
                         (humanize-tokens auto-compact-buffer))
                lines))
        ;; Free space
        (when (plusp free-tokens)
          (push (format nil "  ~A  ~A  ~A tokens"
                         "Free             "
                         (render-ascii-bar (/ free-tokens context-window))
                         (humanize-tokens free-tokens))
                lines))
        ;; Detailed usage from model response
        (when usage-known-p
          (push "" lines)
          (push (format nil "  Input           ~A" (humanize-tokens input-tokens)) lines)
          (push (format nil "  Output          ~A" (humanize-tokens output-tokens)) lines)
          (when (and cache-read (plusp cache-read))
            (push (format nil "  Cache read      ~A" (humanize-tokens cache-read)) lines))
          (when (and cache-write (plusp cache-write))
            (push (format nil "  Cache write     ~A" (humanize-tokens cache-write)) lines))
          (push (format nil "  Total reported  ~A" (humanize-tokens (or total-reported 0))) lines)
          (when (and trailing (plusp trailing))
            (push (format nil "  Trailing est.   ~A" (humanize-tokens trailing)) lines))
          (push (format nil "  Estimated total ~A" (humanize-tokens estimated-total)) lines))
        ;; Model info
        (push "" lines)
        (let ((provider (getf b :provider))
              (model (getf b :model))
              (thinking (getf b :thinking)))
          (push (format nil "  ~A~@[/~A~]~@[ ~(~A~)~]"
                         (or provider "?") model thinking)
                lines))
        ;; Message count
        (push (format nil "  Messages        ~D" (getf b :messages-count)) lines)
        ;; Post-compaction note
        (unless usage-known-p
          (push "" lines)
          (push "  Token counts are unknown until the next model response (post-compaction)." lines))
        (format nil "~{~A~^~%~}" (nreverse lines))))))

;;; ---------------------------------------------------------------------------
;;; Command handler
;;; ---------------------------------------------------------------------------

(defun run-context-command (command arguments context &key call-id on-update)
  (declare (ignore command arguments call-id on-update))
  (reply (context-report context)))

;;; ---------------------------------------------------------------------------
;;; Extension definition
;;; ---------------------------------------------------------------------------

(defextension context
  (:requires
   (capability commands :contract commands/v1)
   (capability agent/session :contract agent/session/v1))
  (:provides
   (command "context"
     :description "Show context window usage: tokens, model, breakdown, and free space."
     :arguments '()
     :handler #'run-context-command)))
