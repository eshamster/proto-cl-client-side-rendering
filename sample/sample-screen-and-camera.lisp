(defpackage sample-proto-cl-client-side-rendering/sample-screen-and-camera
  (:use :cl)
  (:export :start-screen-and-camera-sample
           :stop-screen-and-camera-sample)
  (:import-from :proto-cl-client-side-rendering
                :start-game-loop
                :stop-game-loop
                :draw-circle
                :log-console

                :get-screen-size
                :set-screen-size

                :get-client-id-list
                :*target-client-id-list*
                :key-down-now-p
                :mouse-down-p
                :get-mouse-pos
                :get-wheel-delta-y))
(in-package :sample-proto-cl-client-side-rendering/sample-screen-and-camera)

(defun start-screen-and-camera-sample ()
  (start-game-loop :update-func (lambda () (update))))

(defun stop-screen-and-camera-sample ()
  (stop-game-loop))

;; --- internal --- ;;

(defun update ()
  (let ((id 0))
    (draw-circle :id (incf id)
                 :x 400 :y 300
                 :depth 0
                 :r 40 :color #xffffff
                 :fill-p t)
    (update-screen-size)))

(defparameter *diff-screen-size* 50)
(defparameter *min-screen-width* 400)
(defparameter *max-screen-width* 1600)
(defparameter *min-screen-height* 300)
(defparameter *max-screen-height* 1200)

(defun update-screen-size ()
  (let (new-width old-width new-height old-height)
    (multiple-value-bind (width height) (get-screen-size)
      (setf new-width width
            old-width width
            new-height height
            old-height height)
      (dolist (client-id (get-client-id-list))
        (when (key-down-now-p client-id :up)
          (setf new-height (min *max-screen-height* (+ height *diff-screen-size*))))
        (when (key-down-now-p client-id :down)
          (setf new-height (max *min-screen-height* (- height *diff-screen-size*))))
        (when (key-down-now-p client-id :right)
          (setf new-width (min *max-screen-width* (+ width *diff-screen-size*))))
        (when (key-down-now-p client-id :left)
          (setf new-width (max *min-screen-width* (- width *diff-screen-size*)))))
      (unless (and (= new-width old-width)
                   (= new-height old-height))
        (format t "Screen size: width=~D, height=~D~%" new-width new-height)
        (set-screen-size :width new-width :height new-height)))))
