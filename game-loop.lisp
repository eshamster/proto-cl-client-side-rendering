(defpackage proto-cl-client-side-rendering/game-loop
  (:use :cl)
  (:export :start-game-loop
           :stop-game-loop)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :send-from-server)
  (:import-from :bordeaux-threads
                :make-thread
                :join-thread))
(in-package :proto-cl-client-side-rendering/game-loop)

(defvar *current-frame* 0)
(defvar *stop-game-loop-p* nil)
(defvar *loop-thread* nil)

(defun update ()
  (incf *current-frame*))

(defun send-draw ()
  (send-from-server (format nil "current-frame: ~D" *current-frame*)))

(defun start-game-loop ()
  (stop-game-loop)
  (setf *stop-game-loop-p* nil
        *current-frame* 0)
  (setf *loop-thread*
        (make-thread (lambda ()
                       (loop :do
                            (when *stop-game-loop-p*
                              (return))
                            (update)
                            (send-draw)
                            (sleep 1))))))

(defun stop-game-loop ()
  (setf *stop-game-loop-p* t)
  (when *loop-thread*
    (join-thread *loop-thread*)
    (setf *loop-thread* nil)))
