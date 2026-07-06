;;;; Load ~/.sbclrc on activation, retract with :no-op.

(defun install-sbclrc (protocol contribution context)
  (declare (ignore protocol contribution))
  (let ((init-file (merge-pathnames ".sbclrc" (user-homedir-pathname))))
    (when (probe-file init-file)
      (handler-case
          (progn (load init-file)
                  (notify context "Loaded ~/.sbclrc" :level :info))
         (error (c)
            (notify context (format nil "Failed to load ~~/.sbclrc: ~A" c)
                  :level :warn)))
      :loaded)))

(defextension sbclrc
  (:provides
   (effect load-sbclrc
     #'install-sbclrc
     :no-op)))
