(defpackage proto-cl-client-side-rendering/playground
  (:use :cl
        :proto-cl-client-side-rendering/defines))
(in-package :proto-cl-client-side-rendering/playground)

(defvar.hl x 888)

(defonce.hl once 100)

(defun.hl my-log (text)
  ((ps:@ console log) text))

(with-hot-loads (:label sample)
  (my-log (+ x ": Hello Hot Loading!!")))
