(defpackage proto-cl-client-side-rendering/client/global
  (:use :cl)
  (:export :get-rendered-dom
           :set-rendered-dom

           :get-screen-width
           :get-screen-height
           :get-screen-scale
           :set-screen-size)
  (:import-from :ps-experiment
                :defvar.ps
                :defvar.ps+
                :defun.ps
                :defun.ps+
                :enable-ps-experiment-syntax))
(in-package :proto-cl-client-side-rendering/client/global)

(enable-ps-experiment-syntax)

(defvar.ps+ *rendered-dom* nil)

(defun.ps+ get-rendered-dom ()
  *rendered-dom*)
(defun.ps+ set-rendered-dom (dom)
  (setf *rendered-dom* dom))

;; --- screen size --- ;;

(defvar.ps+ *screen-width* nil)
(defvar.ps+ *screen-height* nil)
(defvar.ps+ *screen-scale* 1)

(defun.ps+ get-screen-width ()
  *screen-width*)
(defun.ps+ get-screen-height ()
  *screen-height*)
(defun.ps+ get-screen-scale ()
  *screen-scale*)

(defun.ps+ set-screen-size (width height scale)
  (setf *screen-width* width
        *screen-height* height
        *screen-scale* scale))
