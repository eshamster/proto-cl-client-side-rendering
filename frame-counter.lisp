(defpackage proto-cl-client-side-rendering/frame-counter
  (:use :cl)
  (:export :incf-frame-count
           :get-frame-count
           :reset-frame-count
           :incf-index-in-frame))
(in-package :proto-cl-client-side-rendering/frame-counter)

(defvar *current-frame* 0)
(defvar *index-in-frame* 0)

(defun reset-frame-count ()
  (setf *current-frame* 0
        *index-in-frame* 0))

(defun incf-frame-count ()
  (incf *current-frame*)
  (setf *index-in-frame* 0))

(defun get-frame-count ()
  *current-frame*)

(defun incf-index-in-frame ()
  (let ((current-index *index-in-frame*))
    (incf *index-in-frame*)
    current-index))
