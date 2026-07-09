(defpackage #:kli/model-indicator
  (:use #:cl)
  (:import-from #:kli
                #:live-object
                #:object-id
                #:context-registry
                #:find-live-object
                #:active-protocol)
  (:import-from #:kli/ext
                #:defextension
                #:protocol-storage
                #:protocol-storage-table
                #:contribution-state)
  (:import-from #:kli/tui/status
                #:set-status)
  (:import-from #:kli/tui/style
                #:style)
  (:import-from #:kli/text
                #:pad-right))

(in-package #:kli/model-indicator)

;;; Model Indicator Extension
;;; Captures the actual model name and content length from OpenAI streaming
;;; chunks and displays per-model usage statistics in the TUI footer.
;;; Format: Model: (* model-a: 23.3%|model-b: 33.3%|model-c: 23.3%)
;;; The current active model is preceded by "*".

;; Per-protocol storage:
;;   - model-stats: hash-table (model-name -> total content length)
;;   - current-model: the model name currently streaming
(defvar *protocol-stats* (make-hash-table :test 'eq)
  "Hash table mapping protocols to their model statistics plist.")

(defun get-protocol-stats (protocol)
  "Get the stats plist for a protocol, creating it if needed."
  (or (gethash protocol *protocol-stats*)
      (setf (gethash protocol *protocol-stats*)
            (list :model-stats (make-hash-table :test #'equal)
                  :current-model nil))))

(defun get-model-stats (protocol)
  "Get the model -> content-length hash table for a protocol."
  (getf (get-protocol-stats protocol) :model-stats))

(defun get-current-model (protocol)
  "Get the currently streaming model name for a protocol."
  (getf (get-protocol-stats protocol) :current-model))

(defun set-current-model (protocol model-name)
  "Set the currently streaming model name for a protocol."
  (let ((stats (get-protocol-stats protocol)))
    (setf (getf stats :current-model) model-name)))

(defun record-model-content (protocol model-name content-length)
  "Accumulate content length for a model."
  (let ((stats (get-model-stats protocol)))
    (incf (gethash model-name stats 0) content-length)))

(defun clear-protocol-stats (protocol)
  "Clear all stats for a protocol."
  (remhash protocol *protocol-stats*))

(defun format-model-stats (protocol &optional theme)
  "Format the model statistics as:
Model: <active-model>[accent](23.3%) <model-2>(33.3%) <model-3>(23.3%)
The 'Model: ' label and the whole body are styled with 'muted'.
The active model is styled with 'accent' (yellow) and placed first,
overriding the outer muted for its span (selective resets restore muted after).
Returns NIL if no stats."
  (let ((stats (get-model-stats protocol))
        (current (get-current-model protocol)))
    (when (plusp (hash-table-count stats))
      (let* ((total (loop for v being the hash-values of stats sum v))
             (entries
              (loop for model being the hash-key of stats
                      using (hash-value len)
                    collect (cons model len))))
        ;; Separate current model from the rest
        (let ((current-entry (find current entries :key #'car :test #'equal))
              (rest-entries (remove current entries :key #'car :test #'equal)))
          ;; Sort remaining by percentage descending
          (setq rest-entries (sort rest-entries #'> :key #'cdr))
          (flet ((format-entry (model len accent-p)
                               (let* ((pct (if (plusp total)
                                               (* 100.0 (/ len total))
                                               0.0))
                                      (text (format nil "~A(~,1F%)" model pct)))
                                 (if (and accent-p theme)
                                     (style theme "accent" text)
                                     text))))
            (let ((parts
                   (append (when current-entry
                             (list (format-entry (car current-entry)
                                                 (cdr current-entry) t)))
                           (loop for (model . len) in rest-entries
                                 collect (format-entry model len nil)))))
              (let ((body (format nil "~{~A~^ ~}" parts)))
                (if theme
                    (format nil "~A ~A"
                            (style theme "text" "Model:")
                            (style theme "muted" body))
                    (format nil "Model: ~A" body))))))))))

(defun make-streaming-model-capturing-wrapper (original-fn protocol)
  "Create a wrapper that captures model name and content length from chunks.
Extracts 'model' and choices[0].delta.content from each SSE JSON chunk."
  (lambda (data-string state emit)
    (when (and data-string (stringp data-string) (plusp (length data-string)))
      (handler-case
          (let ((json (com.inuoe.jzon:parse data-string)))
            (let ((model (gethash "model" json)))
              (when (and model (stringp model) (plusp (length model)))
                ;; Track current model
                (set-current-model protocol model)
                ;; Accumulate content length from choices[0].delta.content
                (let* ((choices (gethash "choices" json))
                       (choice (and (vectorp choices)
                                    (plusp (length choices))
                                    (aref choices 0)))
                       (delta (and (hash-table-p choice)
                                   (gethash "delta" choice)))
                       (content (and (hash-table-p delta)
                                     (gethash "content" delta))))
                  (when (and (stringp content) (plusp (length content)))
                    (record-model-content protocol model (length content)))))))
        (error () nil)))
    (funcall original-fn data-string state emit)))

(defun install-capture-streaming-model (protocol contribution context)
  "Hook into the transport layer's chunk processing to capture model names
and content lengths from streaming chunks."
  (declare (ignore contribution context))
  (let* ((transports-package (find-package :kli/model/transports))
         (sym (and transports-package
                   (find-symbol "MAP-COMPLETIONS-CHUNK" transports-package)))
         (original-fn (and sym (fboundp sym) (symbol-function sym))))
    (when original-fn
      (setf (symbol-function sym)
            (make-streaming-model-capturing-wrapper original-fn protocol))
      (list :original-fn original-fn :symbol sym))))

(defun uninstall-capture-streaming-model (protocol contribution context)
  "Restore the original map-completions-chunk function."
  (declare (ignore protocol context))
  (let ((state (contribution-state contribution)))
    (when state
      (let ((original-fn (getf state :original-fn))
            (sym (getf state :symbol)))
        (when (and original-fn sym)
          (setf (symbol-function sym) original-fn))))))

(defextension model-indicator
  (:requires
   (capability events :contract events/v1))
  (:provides
   (widget streaming-model
      (lambda (protocol theme width)
        (let ((text (format-model-stats protocol theme)))
          (when (and text (plusp (length text)))
            (list (if (<= (length text) width)
                      (pad-right text width)
                      text))))))

   (effect capture-streaming-model
     #'install-capture-streaming-model
     #'uninstall-capture-streaming-model)))
