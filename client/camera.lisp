(defpackage proto-cl-client-side-rendering/client/camera
  (:use :cl)
  (:export :init-camera
           :get-camera
           :set-camera-params)
  (:import-from :parenscript
                :chain
                :new
                :@)
  (:import-from :ps-experiment
                :defun.ps
                :defvar.ps+
                :enable-ps-experiment-syntax))
(in-package :proto-cl-client-side-rendering/client/camera)

(enable-ps-experiment-syntax)

(defvar.ps+ *camera* nil)

(defun.ps init-camera (&key offset-x offset-y width height)
  (let* ((x offset-x)
         (y offset-y)
         (z 1000)
         (camera (new (#j.THREE.OrthographicCamera#
                       0 1 0 1 ; temporal valid values
                       0 (* z 2)))))
    (setf *camera* camera)
    (set-camera-params :offset-x offset-x
                       :offset-y offset-y
                       :width width
                       :height height)
    (camera.position.set 0 0 z)
    camera))

(defun.ps get-camera ()
  *camera*)

(defun.ps set-camera-params (&key (offset-x (* *camera*.left -1))
                                  (offset-y (* *camera*.bottom -1))
                                  (width (- *camera*.right *camera*.left))
                                  (height (- *camera*.top *camera*.bottom)))
  (console.log (+ offset-x ":" offset-y))
  (setf *camera*.left (* offset-x -1)
        *camera*.right (- width offset-x)
        *camera*.bottom (* offset-y -1)
        *camera*.top (- height offset-y))
  (*camera*.update-projection-matrix))
