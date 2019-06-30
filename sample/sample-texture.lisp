(defpackage sample-proto-cl-client-side-rendering/sample-texture
  (:use :cl)
  (:export :start-texture
           :stop-texture)
  (:import-from :proto-cl-client-side-rendering
                :start-game-loop
                :stop-game-loop
                :draw-circle
                :draw-image
                :load-image
                :make-image-uv))
(in-package :sample-proto-cl-client-side-rendering/sample-texture)

(defun start-texture ()
  (load-image :path "sample.png"
              :name :sample)
  (load-image :path "multiple_image.png"
              :name :a
              :uv (make-image-uv :width 0.5))
  (load-image :path "multiple_image.png"
              :name :b
              :uv (make-image-uv :x 0.5 :width 0.5))
  (start-game-loop :update-func (lambda () (update))))

(defun stop-texture ()
  (stop-game-loop))

;; --- internal --- ;;

(defvar *temp-counter* 0)

(defun update ()
  (incf *temp-counter*)
  (let ((id 0))
    (draw-circle :id (incf id)
                 :x 200 :y (+ 300 (* 100 (sin (/ *temp-counter* 2))))
                 :depth 0
                 :r 40 :color #xff0000)
    (draw-image :id (incf id)
                :image-name :sample
                :x 400 :y 300
                :width 50 :height 50
                :rotate (* -1/10 *temp-counter*)
                :depth 0 :color #xffffff)
    (draw-image :id (incf id)
                :image-name :a
                :x 500 :y 300
                :width 50 :height 50
                :rotate (* -1/10 *temp-counter*)
                :depth 0 :color #xffffff)
    (draw-image :id (incf id)
                :image-name :b
                :x 520 :y 300
                :width 50 :height 50
                :rotate (* 1/10 *temp-counter*)
                :depth 0 :color #xffffff)))
