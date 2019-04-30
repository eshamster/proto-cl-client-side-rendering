(defpackage sample-proto-cl-client-side-rendering/game-loop
  (:use :cl)
  (:export :start-sample-game-loop
           :stop-sample-game-loop)
  (:import-from :proto-cl-client-side-rendering
                :start-game-loop
                :stop-game-loop
                :draw-circle))
(in-package :sample-proto-cl-client-side-rendering/game-loop)

(defun start-sample-game-loop ()
  (start-game-loop :update-func #'update))

(defun stop-sample-game-loop ()
  (stop-game-loop))

;; --- internal --- ;;

(defvar *temp-counter* 0)

(defun update ()
  (incf *temp-counter*)
  (draw-circle :id 0
               :x 200 :y (+ 300 (* 100 (sin (/ *temp-counter* 2))))
               :depth 0
               :r 40 :color #xff0000)
  (draw-circle :id 1
               :x 500 :y (+ 300 (* 100 (sin (/ *temp-counter* 3))))
               :depth 0 :fill-p t
               :r 40 :color #x00ffff))
