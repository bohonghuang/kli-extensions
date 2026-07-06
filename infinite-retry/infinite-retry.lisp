 (defun install-infinite-retry (protocol contribution context)
   (declare (ignore protocol contribution context))
   (let* ((registry (kli:context-registry kli/app:*current-context*))
          (session (kli:find-live-object registry :agent-session-service))
          (old-policy (slot-value session 'kli/agent/session::retry-policy)))
     (kli/agent/session:recode-retry-policy session
       :max-attempts 1000000
       :base-delay-ms 1000)
     old-policy))
 
 (defun retract-infinite-retry (protocol contribution context)
   (declare (ignore protocol context))
   (let* ((registry (kli:context-registry kli/app:*current-context*))
          (session (kli:find-live-object registry :agent-session-service))
          (old-policy (kli/ext:contribution-state contribution)))
     (when old-policy
       (setf (slot-value session 'kli/agent/session::retry-policy) old-policy))))
 
 (defextension infinite-retry
   (:provides
    (effect set-infinite-retry
      #'install-infinite-retry
      #'retract-infinite-retry)))
