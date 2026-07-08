;;;; token-indicator.lisp -- cumulative session token counter in the footer.
;;;;
;;;; Displays cumulative output tokens for the current session plus the
;;;; tokens-per-second rate of the last completed request as
;;;; `Token: <output-token> (<tps>TPS)`.  The output count is monotonic --
;;;; it only goes up, never down, even after compaction.  Output tokens are
;;;; additive across requests (each request's output is fresh), so we sum
;;;; the per-request increments.
;;;;
;;;; The handler tracks the last-seen :output-tokens per request-id, so a
;;;; request that emits multiple usage deltas (Anthropic fires one at
;;;; message_start and another at message_stop) counts only the increment.
;;;; A new request-id resets the per-request baseline to zero, so its first
;;;; delta counts in full.  The cumulative total updates after every model
;;;; request -- message or tool-call follow-up -- not just at turn end.
;;;;
;;;; TPS is calculated per request: we stamp the monotonic clock at
;;;; :agent/message-start, track the final output-token count via
;;;; :agent/usage, and at :agent/message-end divide the request's output
;;;; tokens by the elapsed seconds to get tokens-per-second for the last
;;;; request.

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
;;     :total-output   - cumulative output-token count across all requests
;;     :requests       - hash table: request-id -> last-seen output-tokens
;;     :request-starts - hash table: request-id -> monotonic start tick
;;     :last-tps       - TPS (tokens/sec) of the most recently completed request
;; Keyed by protocol so state is isolated per session and survives across
;; turns, the same pattern the model-indicator extension uses.
(defvar *protocol-tokens* (make-hash-table :test 'eq)
  "Hash table mapping protocols to their token-tracking plist.")

(defun get-protocol-state (protocol)
  "Get or create the token-tracking state for PROTOCOL."
  (or (gethash protocol *protocol-tokens*)
      (setf (gethash protocol *protocol-tokens*)
            (list :total-output 0
                  :requests (make-hash-table :test 'eq)
                  :request-starts (make-hash-table :test 'eq)
                  :last-tps nil))))

(defun get-token-counts (protocol)
  "Return (values output last-tps) -- the cumulative output token count and
the TPS of the last completed request for PROTOCOL, both 0/nil when nothing
has been tracked yet."
  (let ((state (get-protocol-state protocol)))
    (values (getf state :total-output)
            (getf state :last-tps))))

(defun clear-token-count (protocol)
  "Clear all token state for PROTOCOL."
  (remhash protocol *protocol-tokens*))

(defun record-request-start (protocol request-id)
  "Stamp the monotonic start tick for REQUEST-ID on PROTOCOL.  No-op when
REQUEST-ID is nil."
  (when request-id
    (let ((state (get-protocol-state protocol)))
      (setf (gethash request-id (getf state :request-starts))
            (get-internal-real-time)))))

(defun record-request-usage (protocol request-id output-tokens)
  "Update PROTOCOL's cumulative output tracking for REQUEST-ID.
OUTPUT-TOKENS is a running total for the request, so we count only the
forward increment since the last delta we saw for the same request-id; a
new request-id starts from zero so its first delta counts in full.
No-op when OUTPUT-TOKENS is nil/missing."
  (when (and request-id (integerp output-tokens))
    (let* ((state (get-protocol-state protocol))
           (requests (getf state :requests))
           (last (gethash request-id requests 0)))
      (when (> output-tokens last)
        (incf (getf state :total-output) (- output-tokens last))
        (setf (gethash request-id requests) output-tokens)))))

(defun record-request-end (protocol request-id)
  "Compute TPS for REQUEST-ID on PROTOCOL and store it as :last-tps.
TPS = request-output-tokens / elapsed-seconds, where the start tick was
stamped by record-request-start and the output count by record-request-usage.
Cleans up per-request tracking entries.  No-op when REQUEST-ID is nil or
no start tick was recorded."
  (when request-id
    (let* ((state (get-protocol-state protocol))
           (starts (getf state :request-starts))
           (start-tick (gethash request-id starts)))
      (when start-tick
        (let* ((requests (getf state :requests))
               (output (gethash request-id requests 0))
               (elapsed-ticks (- (get-internal-real-time) start-tick))
               (elapsed-sec (/ elapsed-ticks
                              (coerce internal-time-units-per-second
                                      'double-float))))
          (when (and output (plusp output) (plusp elapsed-sec))
            (setf (getf state :last-tps) (/ output elapsed-sec)))
          (remhash request-id starts)
          (remhash request-id requests))))))

(defun humanize-token-count (n)
  "N as a compact count: bare integer below 1000, else thousands with one
decimal (dropped when whole).  NIL or 0 renders as 0."
  (cond ((null n) "0")
        ((< n 1000) (format nil "~D" n))
        (t (let ((k (/ n 1000.0)))
             (if (= k (ffloor k))
                 (format nil "~Dk" (truncate k))
                 (format nil "~,1Fk" k))))))

(defun humanize-tps (tps)
  "TPS as a compact number: integer below 10 (no decimal), else one decimal.
NIL renders as empty string (caller decides whether to show the TPS part)."
  (cond ((null tps) "")
        ((< tps 10) (format nil "~D" (round tps)))
        (t (format nil "~,1F" tps))))

(defun format-token-line (protocol &optional theme)
  "Format the footer line: `Token: <output-token> (<tps>TPS)`.  The TPS
parenthetical is omitted when no TPS has been calculated yet.  Returns NIL
when no tokens have been counted yet, so the widget draws no line until
there is something to show."
  (multiple-value-bind (output last-tps)
      (get-token-counts protocol)
    (when (and output (plusp output))
      (let* ((out-str (humanize-token-count output))
             (tps-str (humanize-tps last-tps))
             (tps-part (if (and last-tps (plusp last-tps))
                           (format nil " (~ATPS)" tps-str)
                           ""))
             (body (format nil "~A~A" out-str tps-part)))
        (if theme
            (format nil "~A ~A"
                    (style theme "text" "Token:")
                    (style theme "accent" body))
            (format nil "Token: ~A" body))))))

(defextension token-indicator
  (:provides
   (widget session-tokens
     (lambda (protocol theme width)
       (let ((text (format-token-line protocol theme)))
         (when (and text (plusp (length text)))
           (list (if (<= (length text) width)
                     (pad-right text width)
                     text))))))

   (on :agent/message-start
       (lambda (event context)
         (let ((protocol (active-protocol context))
               (payload (event-payload event)))
           (when protocol
             (record-request-start protocol (getf payload :request-id))))))

   (on :agent/usage
       (lambda (event context)
         (let ((protocol (active-protocol context))
               (payload (event-payload event)))
           (when protocol
             (let ((usage (getf payload :usage)))
               (record-request-usage
                protocol
                (getf payload :request-id)
                (getf usage :output-tokens)))))))

   (on :agent/message-end
       (lambda (event context)
         (let ((protocol (active-protocol context))
               (payload (event-payload event)))
           (when protocol
             (record-request-end protocol (getf payload :request-id))))))))
