 ;;;; Model Indicator — shows per-model usage stats in the TUI footer.
 ;;;; Captures model name + content length from OpenAI streaming chunks.
 ;;;; Format: Model: * model-a(23.3%) model-b(33.3%) model-c(23.3%)

;;; Per-protocol state (protocol → plist of :model-stats :current-model)

(defun get-protocol-stats (protocol)
  (or (kli/ext:ensure-protocol-storage protocol :streaming-model-stats
        (lambda () (list :model-stats (make-hash-table :test #'equal)
                         :current-model nil)))
      ;; Fallback: already initialized, return it
      (let ((storage (kli/ext:protocol-storage protocol)))
        (gethash :streaming-model-stats
                 (kli/ext:protocol-storage-table storage)))))

(defun get-model-stats (protocol)
  (getf (get-protocol-stats protocol) :model-stats))

(defun get-current-model (protocol)
  (getf (get-protocol-stats protocol) :current-model))

(defun set-current-model (protocol model-name)
  (setf (getf (get-protocol-stats protocol) :current-model) model-name))

(defun record-model-content (protocol model-name content-length)
  (incf (gethash model-name (get-model-stats protocol) 0) content-length))

(defun clear-streaming-stats (protocol)
  (let ((stats (get-model-stats protocol)))
    (clrhash stats))
  (set-current-model protocol nil))

;;; Widget — redrawn every frame

(defun format-model-stats (protocol theme)
  (let ((stats (get-model-stats protocol))
        (current (get-current-model protocol)))
    (when (plusp (hash-table-count stats))
      (let* ((total (loop for v being the hash-values of stats sum v))
             (entries (loop for m being the hash-key of stats
                            using (hash-value len)
                            collect (cons m len))))
        (let ((current-entry (find current entries :key #'car :test #'equal))
              (rest-entries (remove current entries :key #'car :test #'equal)))
          (setf rest-entries (sort rest-entries #'> :key #'cdr))
          (flet ((fmt-entry (model len accent-p)
                   (let* ((pct (if (plusp total) (* 100.0 (/ len total)) 0.0))
                          (text (format nil "~A(~,1F%)" model pct)))
                     (if (and accent-p theme)
                         (kli/tui/style:style theme "accent" text)
                         text))))
            (let ((parts (append (when current-entry
                                    (list (fmt-entry (car current-entry)
                                                      (cdr current-entry) t)))
                                  (loop for (model . len) in rest-entries
                                        collect (fmt-entry model len nil)))))
              (let ((body (format nil "~{~A~^ ~}" parts)))
                (if theme
                    (format nil "~A ~A"
                            (kli/tui/style:style theme "text" "Model:")
                            (kli/tui/style:style theme "muted" body))
                    (format nil "Model: ~A" body))))))))))

;;; Effect — hook into chunk processing

(defun install-capture (protocol contribution context)
  (declare (ignore contribution context))
  (let* ((pkg (find-package :kli/model/transports))
         (sym (and pkg (find-symbol "MAP-COMPLETIONS-CHUNK" pkg)))
         (orig (and sym (fboundp sym) (symbol-function sym))))
    (when orig
      (setf (symbol-function sym)
            (lambda (data-string state emit)
              (when (and data-string (stringp data-string) (plusp (length data-string)))
                (handler-case
                  (let ((json (com.inuoe.jzon:parse data-string)))
                    (let ((model (gethash "model" json)))
                      (when (and model (stringp model) (plusp (length model)))
                        (set-current-model protocol model)
                        (let* ((choices (gethash "choices" json))
                               (choice (and (vectorp choices) (plusp (length choices)) (aref choices 0)))
                               (delta (and (hash-table-p choice) (gethash "delta" choice)))
                               (content (and (hash-table-p delta) (gethash "content" delta))))
                          (when (and (stringp content) (plusp (length content)))
                            (record-model-content protocol model (length content)))))))
                  (error () nil)))
              (funcall orig data-string state emit)))
      (list :original-fn orig :symbol sym))))

(defun uninstall-capture (protocol contribution context)
  (declare (ignore protocol context))
  (let ((state (kli/ext:contribution-state contribution)))
    (when state
      (let ((orig (getf state :original-fn))
            (sym (getf state :symbol)))
        (when (and orig sym)
          (setf (symbol-function sym) orig))))))

;;; Event handlers — clear stats at turn boundaries

(defun on-message-end (event context)
  (declare (ignore event))
  (let ((protocol (kli:active-protocol context)))
    (when protocol (clear-streaming-stats protocol))))

(defun on-error (event context)
  (declare (ignore event))
  (let ((protocol (kli:active-protocol context)))
    (when protocol (clear-streaming-stats protocol))))

;;; Extension definition

 (defextension model-indicator
  (:requires
   (capability events :contract events/v1))
  (:provides
   (widget streaming-model
     (lambda (protocol theme width)
       (let ((text (format-model-stats protocol theme)))
         (when (and text (plusp (length text)))
           (list (kli/text:pad-right text width))))))

   (effect capture-streaming-model
     #'install-capture
     #'uninstall-capture)

   (on :agent/message-end #'on-message-end)
   (on :agent/error #'on-error)))
