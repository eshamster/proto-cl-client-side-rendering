(defpackage proto-cl-client-side-rendering/client/frame-counter
  (:use :cl)
  (:export :update-frame-counter
           :get-frame-count)
  (:import-from :ps-experiment
                :defvar.ps+
                :defun.ps+
                :enable-ps-experiment-syntax))
(in-package :proto-cl-client-side-rendering/client/frame-counter)

(enable-ps-experiment-syntax)

(defvar.ps+ *frame-count* 0)

(defun.ps+ update-frame-counter ()
  (incf *frame-count*))

(defun.ps+ get-frame-count ()
  *frame-count*)
