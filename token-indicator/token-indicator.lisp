;;;; token-indicator.lisp -- cumulative session token counter in the footer.
;;;;
;;;; Displays cumulative input and output tokens for the current session as
;;;; `Tokens: <input>(<cache>)|<output>`.  Output tokens are additive across
;;;; requests (each request's output is fresh), so we sum the per-request
;;;; increments.  Input tokens and cache-read tokens subsume the entire
;;;; conversation context so far, so we track the latest value (the high-water
;;;; mark) rather than summing -- summing would double-count prior context on
;;;; every follow-up request.  The cache-read portion is shown in parentheses
;;;; after the input count; the parenthetical is omitted when zero.
;;;;
;;;; The handler tracks the last-seen :output-tokens per request-id, so a
;;;; request that emits multiple usage deltas (Anthropic fires one at
;;;; message_start and another at message_stop) counts only the increment.
;;;; A new request-id resets the per-request baseline to zero, so its first
;;;; delta counts in full.  The cumulative total updates after every model
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
;;     :total-output  - cumulative output-token count across all requests
;;     :max-input     - high-water mark of input-tokens (latest request's value)
;;     :max-cache     - high-water mark of cache-read-tokens (latest request's)
;;     :requests      - hash table: request-id -> last-seen output-tokens for it
;; Keyed by protocol so state is isolated per session and survives across
;; turns, the same pattern the model-indicator extension uses.
(defvar *protocol-tokens* (make-hash-table :test 'eq)
  "Hash table mapping protocols to their token-tracking plist.")

(defun get-protocol-state (protocol)
  "Get or create the token-tracking state for PROTOCOL."
  (or (gethash protocol *protocol-tokens*)
      (setf (gethash protocol *protocol-tokens*)
            (list :total-output 0
                  :max-input 0
                  :max-cache 0
                  :requests (make-hash-table :test 'eq)))))

(defun get-token-counts (protocol)
  "Return (values input cache-read output) -- the latest input and cache-read
token counts (high-water marks) and the cumulative output token count for
PROTOCOL, all 0 when nothing has been tracked yet."
  (let ((state (get-protocol-state protocol)))
    (values (getf state :max-input)
            (getf state :max-cache)
            (getf state :total-output))))

(defun clear-token-count (protocol)
  "Clear all token state for PROTOCOL."
  (remhash protocol *protocol-tokens*))

(defun record-request-usage (protocol request-id input-tokens cache-read-tokens output-tokens)
  "Update PROTOCOL's token tracking for REQUEST-ID.  OUTPUT-TOKENS is a running
total for the request, so we count only the forward increment since the last
delta we saw for the same request-id; a new request-id starts from zero so its
first delta counts in full.  INPUT-TOKENS and CACHE-READ-TOKENS reflect the
full context sent with the request, so we keep the high-water mark across all
requests rather than summing.  No-op when all values are nil/missing."
  (when (and request-id
             (or (and input-tokens (integerp input-tokens))
                 (and cache-read-tokens (integerp cache-read-tokens))
                 (and output-tokens (integerp output-tokens))))
    (let* ((state (get-protocol-state protocol))
           (requests (getf state :requests)))
      ;; Input: keep the high-water mark (latest request's context size).
      (when (and input-tokens (integerp input-tokens)
                 (> input-tokens (getf state :max-input)))
        (setf (getf state :max-input) input-tokens))
      ;; Cache-read: keep the high-water mark (latest request's cache hits).
      (when (and cache-read-tokens (integerp cache-read-tokens)
                 (> cache-read-tokens (getf state :max-cache)))
        (setf (getf state :max-cache) cache-read-tokens))
      ;; Output: accumulate the forward increment for this request.
      (when (and output-tokens (integerp output-tokens))
        (let ((last (gethash request-id requests 0)))
          (when (> output-tokens last)
            (incf (getf state :total-output) (- output-tokens last))
            (setf (gethash request-id requests) output-tokens)))))))

(defun humanize-token-count (n)
  "N as a compact count: bare integer below 1000, else thousands with one
decimal (dropped when whole).  NIL or 0 renders as 0."
  (cond ((null n) "0")
        ((< n 1000) (format nil "~D" n))
        (t (let ((k (/ n 1000.0)))
             (if (= k (ffloor k))
                 (format nil "~Dk" (truncate k))
                 (format nil "~,1Fk" k))))))

(defun format-token-line (protocol &optional theme)
  "Format the footer line: `Tokens: <input>(<cache>)|<output>`.  The cache
parenthetical is omitted when cache-read is zero.  Returns NIL when no tokens
have been counted yet, so the widget draws no line until there is something
to show."
  (multiple-value-bind (input cache output)
      (get-token-counts protocol)
    (when (or (and input (plusp input))
              (and cache (plusp cache))
              (and output (plusp output)))
      (let* ((in-str (humanize-token-count input))
             (cache-str (if (and cache (plusp cache))
                            (format nil "(~A)" (humanize-token-count cache))
                            ""))
             (out-str (humanize-token-count output))
             (body (format nil "~A~A|~A" in-str cache-str out-str)))
        (if theme
            (format nil "~A ~A"
                    (style theme "text" "Tokens:")
                    (style theme "accent" body))
            (format nil "Tokens: ~A" body))))))

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
             (let ((usage (getf payload :usage)))
               (record-request-usage
                protocol
                (getf payload :request-id)
                (getf usage :input-tokens)
                (getf usage :cache-read-tokens)
                (getf usage :output-tokens)))))))))
