;;;; srfi-45.asd

(cl:in-package :asdf)


(defsystem :srfi-45
  :version "20200406"
  :description "SRFI 45 for CL: Primitives for Expressing Iterative Lazy Algorithms"
  :long-description "SRFI 45 for CL: Primitives for Expressing Iterative Lazy Algorithms
https://srfi.schemers.org/srfi-45"
  :author "Andre van Tonder"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:trivial-garbage
               :mbe
               :fiveam)
  :components ((:file "package")
               (:file "srfi-45")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-45))))
  (let ((name "https://github.com/g000001/srfi-45")
        (nickname :srfi-45))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-45))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-45#internals"))
        (*print-case* :upcase))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-45)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
