(defpackage proto-cl-client-side-rendering/game-loop
  (:use :cl)
  (:export :start-game-loop
           :stop-game-loop)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :send-from-server))
(in-package :proto-cl-client-side-rendering/game-loop)

(defun update ()
  (incf *current-frame*))

(defun send-draw ()
  (send-from-server (format nil "current-frame: ~D" *current-frame*)))

(defvar *current-frame* 0)
(defvar *stop-game-loop-p* nil)

;; TODO: run in another thread
(defun start-game-loop ()
  (setf *stop-game-loop-p* nil
        *current-frame* 0)
  (loop do
       (when *stop-game-loop-(return))
       (update)
       (send-draw)
       (sleep 1)))

(defun stop-game-loop ()
  (setf *stop-game-loop-p* t))
