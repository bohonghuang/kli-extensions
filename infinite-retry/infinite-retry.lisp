;;; infinite-retry.lisp — constant-interval retry policy.
;;;
;;; Replaces the session's default retry policy (exponential backoff:
;;; base-delay * 2^attempt, i.e. 1s 2s 4s 8s ...) with one that retries
;;; indefinitely at a constant base-delay interval.

(defparameter *base-delay-ms* 1000
  "Constant retry interval in milliseconds.")

(defun make-constant-retry-policy (max-attempts base-delay-ms retryable-p)
  "A retry policy that waits a constant BASE-DELAY-MS between every attempt,
instead of the built-in exponential backoff."
  (lambda (operation &rest arguments)
    (case operation
      (:inspect (list :max-attempts  max-attempts
                      :base-delay-ms base-delay-ms
                      :retryable-p   retryable-p))
      (:should-retry
       (destructuring-bind (error-payload attempt) arguments
         (cond
           ((>= attempt max-attempts) (values nil nil))
           ((not (kli/ext:safely-invoke retryable-p
                                        :session-policy '(:retry :retryable-p)
                                        error-payload))
            (values nil nil))
           ;; Constant interval — no (expt 2 attempt) dampening.
           (t (values t base-delay-ms))))))))

(defun install-infinite-retry (protocol contribution context)
  (declare (ignore protocol contribution context))
  (let* ((registry (kli:context-registry kli/app:*current-context*))
         (session (kli:find-live-object registry :agent-session-service))
         (old-policy (slot-value session 'kli/agent/session::retry-policy)))
    (setf (kli/agent/session:session-retry-policy session)
          (make-constant-retry-policy
           1000000                  ; max-attempts: effectively infinite
           *base-delay-ms*          ; constant interval
           #'kli/agent/session:transient-model-error-p))
    old-policy))

(defun retract-infinite-retry (protocol contribution context)
  (declare (ignore protocol context))
  (let* ((registry (kli:context-registry kli/app:*current-context*))
         (session (kli:find-live-object registry :agent-session-service))
         (old-policy (kli/ext:contribution-state contribution)))
    (when old-policy
      (setf (kli/agent/session:session-retry-policy session) old-policy))))

(defextension infinite-retry
  (:provides
   (effect set-infinite-retry
     #'install-infinite-retry
     #'retract-infinite-retry)))
