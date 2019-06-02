(defpackage proto-cl-client-side-rendering/client/camera
  (:use :cl)
  (:export :init-camera)
  (:import-from :parenscript
                :chain
                :new
                :@)
  (:import-from :ps-experiment
                :defun.ps
                :enable-ps-experiment-syntax))
(in-package :proto-cl-client-side-rendering/client/camera)

(enable-ps-experiment-syntax)

(defun.ps init-camera (offset-x offset-y width height)
  (let* ((x offset-x)
         (y offset-y)
         (z 1000)
         (camera (new (#j.THREE.OrthographicCamera#
                       (* x -1) (- width x)
                       (- height y) (* y -1)
                       0 (* z 2)))))
    (camera.position.set 0 0 z)
    camera))
