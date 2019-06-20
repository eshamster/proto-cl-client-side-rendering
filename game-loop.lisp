(defpackage proto-cl-client-side-rendering/game-loop
  (:use :cl)
  (:export :start-game-loop
           :stop-game-loop
           :get-fps
           :set-fps
           :log-console)
  (:import-from :proto-cl-client-side-rendering/camera
                :update-camera-info)
  (:import-from :proto-cl-client-side-rendering/client-list-manager
                :update-client-list
                :get-new-client-id-list)
  (:import-from :proto-cl-client-side-rendering/frame-counter
                :incf-frame-count
                :get-frame-count
                :reset-frame-count
                :incf-index-in-frame)
  (:import-from :proto-cl-client-side-rendering/graphics
                :update-graphics)
  (:import-from :proto-cl-client-side-rendering/input
                :update-input)
  (:import-from :proto-cl-client-side-rendering/protocol
                :send-frame-start
                :send-delete-draw-object
                :send-draw-rect
                :send-draw-circle
                :send-draw-line
                :send-draw-arc
                :send-log-console
                :send-frame-end)
  (:import-from :proto-cl-client-side-rendering/screen-size
                :update-screen-size)
  (:import-from :proto-cl-client-side-rendering/texture
                :update-texture)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :send-from-server
                :*target-client-id-list*)
  (:import-from :alexandria
                :make-keyword
                :with-gensyms)
  (:import-from :bordeaux-threads
                :make-thread
                :join-thread))
(in-package :proto-cl-client-side-rendering/game-loop)

;; --- data --- ;;

(defvar *stop-game-loop-p* nil)
(defvar *loop-thread* nil)
(defvar *FPS* 10)

;; --- internal macro --- ;;

(defmacro with-interval ((target-sec) &body body)
  (with-gensyms (before after g-target-sec)
    `(let ((,before (get-current-sec))
           (,g-target-sec ,target-sec))
       ,@body
       (let ((,after (get-current-sec)))
         (when (< (- ,after ,before) ,g-target-sec)
           (sleep (- ,g-target-sec (- ,after ,before))))))))

;; --- interface --- ;;

(defun start-game-loop (&key (update-func (lambda ())))
  (stop-game-loop)
  (setf *stop-game-loop-p* nil)
  (reset-frame-count)
  (setf *loop-thread*
        (make-thread (lambda ()
                       (loop :do
                            (with-interval ((/ 1 *FPS*))
                              (when *stop-game-loop-p*
                                (return))
                              (update-client-list)
                              (update-texture)
                              (incf-frame-count)
                              (update-camera-info)
                              (update-input)
                              (unwind-protect
                                   (progn
                                     (send-frame-start (get-frame-count) (incf-index-in-frame))
                                     (update-screen-size)
                                     (funcall update-func)
                                     (update-graphics))
                                (send-frame-end (get-frame-count) (incf-index-in-frame)))))))))

(defun stop-game-loop ()
  (setf *stop-game-loop-p* t)
  (when *loop-thread*
    (join-thread *loop-thread*)
    (setf *loop-thread* nil)))

(defun get-fps ()
  "Get FPS (frame per second) of game loop."
  *FPS*)

(defun set-fps (fps)
  "Set FPS (frame per second) of game loop."
  (setf *FPS* fps))

;; --- utils --- ;;

(defun get-current-sec ()
  (/ (get-internal-real-time) internal-time-units-per-second))

;; --- sender --- ;;

;; TODO: Consider more proper package

(defun log-console (&key message)
  (send-log-console (get-frame-count) (incf-index-in-frame)
                    :message message))
