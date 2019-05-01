(defpackage proto-cl-client-side-rendering/game-loop
  (:use :cl)
  (:export :start-game-loop
           :stop-game-loop
           :draw-rect
           :draw-circle)
  (:import-from :proto-cl-client-side-rendering/input
                :update-input)
  (:import-from :proto-cl-client-side-rendering/protocol
                :send-frame-start
                :send-draw-rect
                :send-draw-circle
                :send-frame-end)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :send-from-server)
  (:import-from :bordeaux-threads
                :make-thread
                :join-thread))
(in-package :proto-cl-client-side-rendering/game-loop)

(defvar *current-frame* 0)
(defvar *index-in-frame* 0)
(defvar *stop-game-loop-p* nil)
(defvar *loop-thread* nil)

(defun start-game-loop (&key (update-func (lambda ())))
  (stop-game-loop)
  (setf *stop-game-loop-p* nil
        *current-frame* 0)
  (setf *loop-thread*
        (make-thread (lambda ()
                       (loop :do
                            (when *stop-game-loop-p*
                              (return))
                            (setf *index-in-frame* 0)
                            (incf *current-frame*)
                            (update-input)
                            (unwind-protect
                                 (progn
                                   (send-frame-start *current-frame* (incf *index-in-frame*))
                                   (funcall update-func))
                              (send-frame-end *current-frame* (incf *index-in-frame*)))
                            (sleep 0.5))))))

(defun stop-game-loop ()
  (setf *stop-game-loop-p* t)
  (when *loop-thread*
    (join-thread *loop-thread*)
    (setf *loop-thread* nil)))

;; --- sender --- ;;

;; TODO: Consider more proper package

(defun draw-circle (&key id x y depth color fill-p r)
  (send-draw-circle *current-frame* (incf *index-in-frame*)
                    :id id :x x :y y :depth depth
                    :color color :fill-p fill-p :r r))

(defun draw-rect (&key id x y depth color fill-p width height rotate)
  (send-draw-rect *current-frame* (incf *index-in-frame*)
                  :id id :x x :y y :depth depth
                  :color color :fill-p fill-p
                  :width width :height height :rotate rotate))
