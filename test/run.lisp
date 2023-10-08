;;;; run.lisp

(in-package #:moongate/test)

(defun run-all-tests (&optional
                        (*print-pretty* t)
                        (nst:*debug-on-error* t)
                        (nst:*debug-on-fail* nil))
  (nst:nst-cmd :run-package #.*package*))
