;;;; token-indicator.lisp -- cumulative session token counter in the footer.
;;;;
;;;; Displays the running total of output tokens consumed by the entire
;;;; current session. The agent loop runs one model request per turn, and
;;;; loops back for another request after each batch of tool calls, so a
;;;; single user turn with tool calls produces several model requests. Each
;;;; request emits :agent/usage deltas carrying the request's running output
;;;; token count; :output-tokens is additive across requests (each request's
;;;; output is fresh), unlike :input-tokens/:total-tokens which subsume prior
;;;; context and would double-count.
;;;;
;;;; The handler tracks the last-seen :output-tokens per request-id, so a
;;;; request that emits multiple usage deltas (Anthropic fires one at
;;;; message_start and another at message_stop) counts only the increment.
;;;; A new request-id resets the per-request baseline to zero, so its first
;;;; delta counts in full. The cumulative total updates after every model
;;;; request -- message or tool-call follow-up -- not just at turn end.

(defpackage #:kli/token-indicator
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol)
  (:import-from #:kli/ext
                #:defextension)
  (:import-from #:kli/event
                #:event-payload)
  (:import-from #:kli/tui/style
                #:style)
  (:import-from #:kli/text
                #:pad-right))

(in-package #:kli/token-indicator)

;; Per-protocol storage:
;;   protocol -> plist of:
;;     :total    - cumulative output-token count across all requests
;;     :requests - hash table: request-id -> last-seen output-tokens for it
;; Keyed by protocol so state is isolated per session and survives across
;; turns, the same pattern the model-indicator extension uses.
(defvar *protocol-tokens* (make-hash-table :test 'eq)
  "Hash table mapping protocols to their token-tracking plist.")

(defun get-protocol-state (protocol)
  "Get or create the token-tracking state for PROTOCOL."
  (or (gethash protocol *protocol-tokens*)
      (setf (gethash protocol *protocol-tokens*)
            (list :total 0
                  :requests (make-hash-table :test 'eq)))))

(defun get-token-count (protocol)
  "Current cumulative output-token count for PROTOCOL, or 0."
  (getf (get-protocol-state protocol) :total))

(defun clear-token-count (protocol)
  "Clear all token state for PROTOCOL."
  (remhash protocol *protocol-tokens*))

(defun record-request-usage (protocol request-id output-tokens)
  "Accumulate the incremental output-tokens for REQUEST-ID into PROTOCOL's
total. Each request's usage delta carries a running total for that request,
so we count only the delta since the last delta we saw for the same
request-id. A new request-id starts from zero, so its first delta counts in
full. No-op when OUTPUT-TOKENS is nil/missing."
  (when (and request-id output-tokens (integerp output-tokens))
    (let* ((state (get-protocol-state protocol))
           (requests (getf state :requests))
           (last (gethash request-id requests 0)))
      ;; Only count the forward increment; a delta that did not advance the
      ;; request's running total contributes nothing.
      (when (> output-tokens last)
        (incf (getf state :total) (- output-tokens last))
        (setf (gethash request-id requests) output-tokens)))))

(defun humanize-token-count (n)
  "N as a compact count: bare integer below 1000, else thousands with one
decimal (dropped when whole)."
  (cond ((null n) "0")
        ((< n 1000) (format nil "~D" n))
        (t (let ((k (/ n 1000.0)))
             (if (= k (ffloor k))
                 (format nil "~Dk" (truncate k))
                 (format nil "~,1Fk" k))))))

(defun format-token-line (protocol &optional theme)
  "Format the footer line: `Tokens: <count>`. Returns NIL when no tokens have
been counted yet, so the widget draws no line until there is something to show."
  (let ((count (get-token-count protocol)))
    (when (plusp count)
      (let ((text (format nil "Tokens: ~A" (humanize-token-count count))))
        (if theme
            (format nil "~A ~A"
                    (style theme "text" "Tokens:")
                    (style theme "accent" (humanize-token-count count)))
            text)))))

(defextension token-indicator
  (:provides
   (widget session-tokens
     (lambda (protocol theme width)
       (let ((text (format-token-line protocol theme)))
         (when (and text (plusp (length text)))
           (list (if (<= (length text) width)
                     (pad-right text width)
                     text))))))

   (on :agent/usage
       (lambda (event context)
         (let ((protocol (active-protocol context))
               (payload (event-payload event)))
           (when protocol
             (record-request-usage
              protocol
              (getf payload :request-id)
              (getf (getf payload :usage) :output-tokens))))))))
