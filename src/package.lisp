(defpackage llvm
  (:use :cl :cffi)
  (:shadow :constantp :type-of))

(let ((pack (find-package :llvm)))
  (do-all-symbols (sym pack)
    (when (and (not (member sym '(constantp type-of)))
               (eql (symbol-package sym) pack))
      (export sym))))


