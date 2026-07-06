;;;; /context — display context window usage in the message window
;;;; Modelled on @oh-my-pi/pi-coding-agent's /context report.

(defun humanize-tokens (n)
  "Compact token count: bare integer below 1k, else N.Nk."
  (cond ((null n) "?")
        ((< n 1000) (format nil "~D" n))
        (t (let ((k (/ n 1000.0)))
             (if (= k (ffloor k))
                 (format nil "~Dk" (truncate k))
                 (format nil "~,1Fk" k))))))

(defun context-report (context)
  "Build a multi-line context usage report from the live session."
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
         ;; Agent-context → projected messages
         (agent-context (kli/agent/session:agent-session-context
                         service mode-id context))
         (messages (when agent-context
                     (kli/context/lens:context-projected-messages agent-context)))
         ;; Token accounting
         (total-reported (and usage (kli/agent/session:usage-total-tokens usage)))
         (input-tokens  (and usage (kli/agent/session:usage-input-tokens usage)))
         (output-tokens (and usage (kli/agent/session:usage-output-tokens usage)))
         (cache-read    (and usage (kli/agent/session:usage-cache-read-tokens usage)))
         (cache-write   (and usage (kli/agent/session:usage-cache-write-tokens usage)))
         ;; Trailing estimate (unreported in-flight text)
         (trailing (kli/tui/status:trailing-token-estimate messages))
         (estimated-total (+ (or total-reported 0) (or trailing 0)))
         ;; Model info
         (provider (and info (getf info :provider)))
         (model    (and info (getf info :model)))
         (thinking (and info (getf info :thinking)))
         ;; Compaction validity — entries via session-branch
         (store (kli:find-live-object registry :session-store))
         (session-id (and info (getf info :id)))
         (session (and store session-id
                       (kli/session/log:find-session store session-id)))
         (entries (and store session
                       (kli/session/log:session-branch store session nil)))
         (usage-known-p (not (kli/tui/status:usage-unknown-after-compaction-p
                              (or entries '()))))
         ;; Percent
         (pct (when (and context-window (plusp context-window) usage-known-p)
                (* 100.0 (/ estimated-total context-window)))))
    ;; Build lines
    (with-output-to-string (out)
      ;; Header
      (cond
        ((and context-window (plusp context-window))
         (format out "Context window: ~A tokens (~,1f% used)~%"
                 (humanize-tokens context-window) (or pct 0.0)))
        (estimated-total
         (format out "Context: ~A tokens (no window declared)~%"
                 (humanize-tokens estimated-total)))
        (t
         (format out "Context usage is unavailable: ~
                      no model is selected for this session.~%")))
      ;; Model line
      (when (or provider model)
        (format out "  ~A~@[/~A~]~@[ ~(~A~)~]~%"
                 (or provider "?") model thinking))
      ;; Token breakdown
      (when usage-known-p
        (when input-tokens
          (format out "  Input           ~A~%" (humanize-tokens input-tokens)))
        (when output-tokens
          (format out "  Output          ~A~%" (humanize-tokens output-tokens)))
        (when (and cache-read (plusp cache-read))
          (format out "  Cache read      ~A~%" (humanize-tokens cache-read)))
        (when (and cache-write (plusp cache-write))
          (format out "  Cache write     ~A~%" (humanize-tokens cache-write)))
        ;; Total + trailing
        (format out "  Total reported  ~A~%" (humanize-tokens (or total-reported 0)))
        (when (and trailing (plusp trailing))
          (format out "  Trailing est.   ~A~%" (humanize-tokens trailing)))
        (format out "  Estimated total ~A~%" (humanize-tokens estimated-total)))
      ;; Free tokens
      (when (and context-window (plusp context-window) usage-known-p)
        (let ((free (- context-window estimated-total)))
          (format out "  Free            ~A~%" (humanize-tokens (max 0 free)))))
      ;; Unknown-usage note
      (unless usage-known-p
        (format out "  Token counts are unknown until the next model ~
                      response (post-compaction).~%"))
      ;; Message count
      (when messages
        (format out "  Messages        ~D~%" (length messages))))))

(defun run-context-command (command arguments context &key call-id on-update)
  (declare (ignore command arguments call-id on-update))
  (reply (context-report context)))

(defextension context
  (:requires
   (capability commands :contract commands/v1)
   (capability agent/session :contract agent/session/v1))
  (:provides
   (command "context"
     :description "Show context window usage: tokens, model, breakdown, and free space."
     :arguments '()
     :handler #'run-context-command)))
