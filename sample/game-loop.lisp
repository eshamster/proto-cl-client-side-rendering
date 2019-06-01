(defpackage sample-proto-cl-client-side-rendering/game-loop
  (:use :cl)
  (:export :start-sample-game-loop
           :stop-sample-game-loop)
  (:import-from :proto-cl-client-side-rendering
                :start-game-loop
                :stop-game-loop
                :draw-rect
                :draw-circle
                :draw-line
                :draw-arc
                :log-console

                :get-client-id-list
                :*target-client-id-list*
                :key-down-p
                :mouse-down-p
                :get-mouse-pos
                :get-wheel-delta-y
                :touch-summary-down-p
                :get-touch-summary-pos))
(in-package :sample-proto-cl-client-side-rendering/game-loop)

(defun start-sample-game-loop ()
  (start-game-loop :update-func (lambda () (update))))

(defun stop-sample-game-loop ()
  (stop-game-loop))

;; --- internal --- ;;

(defvar *temp-counter* 0)

(defvar *temp-x* 100)
(defvar *temp-y* 300)
(defparameter *temp-speed* 10)

(defvar *temp-r* 50)
(defvar *temp-max-r* 100)
(defvar *temp-min-r* 10)

(defun update ()
  (incf *temp-counter*)
  (let ((id 0))
    (draw-circle :id (incf id)
                 :x 200 :y (+ 300 (* 100 (sin (/ *temp-counter* 2))))
                 :depth 0
                 :r 40 :color #xff0000)
    (draw-circle :id (incf id)
                 :x 300 :y (+ 300 (* 100 (sin (/ *temp-counter* 3))))
                 :depth 0 :fill-p t
                 :r 40 :color #x00ffff)
    (draw-rect :id (incf id)
               :x 400 :y 300
               :depth 0 :fill-p nil
               :width 20 :height 40
               :rotate (* 1/5 *temp-counter*)
               :color #x00ff00)
    (draw-rect :id (incf id)
               :x 500 :y 300
               :depth 0 :fill-p t
               :width 20 :height 40
               :rotate (* -1/5 *temp-counter*)
               :color #xff00ff)
    (draw-rect :id (incf id)
               :x 600 :y 300
               :depth 0 :fill-p nil
               :width 20 :height (+ 40 (* 20 (sin (/ *temp-counter* 2))))
               :rotate 0
               :color #xff00ff)
    (draw-arc :id (incf id)
              :x 100 :y 400
              :depth 0 :r 40
              :start-angle (+ (* PI 1/6) (* 1/6 *temp-counter*))
              :sweep-angle (* PI 1/3)
              :color #x00ffff)
    ;; (log-console :message "test") ; try logging
    (try-keyboard)
    (try-mouse)
    (try-touch)
    (draw-circle :id (incf id)
                 :x *temp-x* :y *temp-y*
                 :depth 10 :fill-p t
                 :r *temp-r* :color #xffffff)
    (draw-line :id (incf id)
               :x1 20 :y1 30
               :x2 500 :y2 60
               :depth -10
               :color #x00ffff)
    ;; try sending to each client
    (flet ((try-send (y color)
             (draw-circle :id (incf id)
                 :x 700 :y y
                 :depth 10 :fill-p t
                 :r 25 :color color)))
      (let ((id-list (get-client-id-list)))
        (let ((*target-client-id-list*
               (remove-if (lambda (id) (= (mod id 2) 0)) id-list)))
          (try-send 500 #xff0000))
        (let ((*target-client-id-list*
               (remove-if (lambda (id) (= (mod id 2) 1)) id-list)))
          (try-send 450 #x0000ff))))))

(defun try-keyboard ()
  (dolist (client-id (get-client-id-list))
    (when (key-down-p client-id :up)
      (incf *temp-y* *temp-speed*))
    (when (key-down-p client-id :down)
      (decf *temp-y* *temp-speed*))
    (when (key-down-p client-id :right)
      (incf *temp-x* *temp-speed*))
    (when (key-down-p client-id :left)
      (decf *temp-x* *temp-speed*))))

(defun try-mouse ()
  (dolist (client-id (get-client-id-list))
    (when (mouse-down-p client-id :left)
      (multiple-value-bind (x y) (get-mouse-pos client-id)
        (setf *temp-x* x
              *temp-y* y)))
    (let ((delta-y (get-wheel-delta-y client-id))
          (diff 10))
      (cond ((< delta-y 0) (incf *temp-r* diff))
            ((> delta-y 0) (decf *temp-r* diff))))))

(defun try-touch ()
  (dolist (client-id (get-client-id-list))
    (when (touch-summary-down-p client-id)
      (multiple-value-bind (x y) (get-touch-summary-pos client-id)
        (assert (and x y))
        (setf *temp-x* x
              *temp-y* y)))))
