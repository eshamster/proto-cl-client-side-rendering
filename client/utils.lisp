(defpackage proto-cl-client-side-rendering/client/utils
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :with-command-data)
  (:import-from :alexandria
                :make-keyword))
(in-package :proto-cl-client-side-rendering/client/utils)

;; --- interface --- ;;

(defmacro.ps+ with-command-data ((&rest target-list) command &body body)
  `(let (,@(mapcar (lambda (target)
                     `(,target (get-data ,command ,(make-keyword target))))
                   target-list))
     ,@body))


;; --- internal --- ;;

(defun.ps+ get-data (command target)
  (gethash target (gethash :data command)))
