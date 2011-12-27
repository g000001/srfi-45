;;;; srfi-45.asd

(cl:in-package :asdf)

(defsystem :srfi-45
  :serial t
  :depends-on (:mbe :fiveam)
  :components ((:file "package")
               (:file "srfi-45")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-45))))
  (load-system :srfi-45)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-45.internal :srfi-45))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
