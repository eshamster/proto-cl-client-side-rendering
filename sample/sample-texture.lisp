(defpackage sample-proto-cl-client-side-rendering/sample-texture
  (:use :cl)
  (:export :start-texture
           :stop-texture)
  (:import-from :proto-cl-client-side-rendering
                :start-game-loop
                :stop-game-loop
                :draw-circle
                :draw-image
                :draw-text
                :load-texture
                :load-image
                :make-image-uv
                :load-font
                :calc-text-width))
(in-package :sample-proto-cl-client-side-rendering/sample-texture)

(defun start-texture ()
  (load-texture :name :sample
                :path "sample.png")
  (load-image :image-name :sample
              :texture-name :sample)

  (load-texture :name :sample-with-alpha
                :path "sample.png"
                :alpha-path "sample_alpha.png")
  (load-image :image-name :sample-with-alpha
              :texture-name :sample-with-alpha)

  (load-texture :name :multiple-image
                :path "multiple_image.png"
                :alpha-path "multiple_image_alpha.png")
  (load-image :image-name :a
              :texture-name :multiple-image
              :uv (make-image-uv :width 0.5))
  (load-image :image-name :b
              :texture-name :multiple-image
              :uv (make-image-uv :x 0.5 :width 0.5))

  (load-texture :name :sample-font
                :path "font.png"
                :alpha-path "font_alpha.png")
  (load-font :name :sample-font
             :texture-name :sample-font
             :json-path "font.json")

  (start-game-loop :update-func (lambda () (update))))

(defun stop-texture ()
  (stop-game-loop))

;; --- internal --- ;;

(defvar *temp-counter* 0)

(defun update ()
  (incf *temp-counter*)
  (let ((id 0))
    (draw-image :id (incf id)
                :image-name :sample
                :x 400 :y 300
                :width 50 :height 50
                :rotate (* -1/10 *temp-counter*)
                :depth 0 :color #xffffff)
    (draw-image :id (incf id)
                :image-name :sample-with-alpha
                :x 400 :y 200
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
                :depth 0 :color #xffffff)
    (let ((height 60)
          (text "Press z/Z key"))
      (draw-text :id (incf id)
                 :text text
                 :font-name :sample-font
                 :x 50 :y 50
                 :width (calc-text-width :font-name :sample-font
                                         :text text
                                         :height height)
                 :height height
                 :depth 0 :color #xffffff))))
