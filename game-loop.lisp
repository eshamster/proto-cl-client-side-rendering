(defpackage proto-cl-client-side-rendering/game-loop
  (:use :cl)
  (:export :start-game-loop
           :stop-game-loop
           :log-console)
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
  (:import-from :proto-cl-client-side-rendering/ws-server
                :send-from-server
                :*target-client-id-list*)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :bordeaux-threads
                :make-thread
                :join-thread))
(in-package :proto-cl-client-side-rendering/game-loop)

(defvar *stop-game-loop-p* nil)
(defvar *loop-thread* nil)

(defun start-game-loop (&key (update-func (lambda ())))
  (stop-game-loop)
  (setf *stop-game-loop-p* nil)
  (reset-frame-count)
  (setf *loop-thread*
        (make-thread (lambda ()
                       (loop :do
                            (when *stop-game-loop-p*
                              (return))
                            (update-client-list)
                            (incf-frame-count)
                            (update-input)
                            (unwind-protect
                                 (progn
                                   (send-frame-start (get-frame-count) (incf-index-in-frame))
                                   (funcall update-func)
                                   (update-graphics))
                              (send-frame-end (get-frame-count) (incf-index-in-frame)))
                            (update-client-list)
                            (sleep 0.5))))))

(defun stop-game-loop ()
  (setf *stop-game-loop-p* t)
  (when *loop-thread*
    (join-thread *loop-thread*)
    (setf *loop-thread* nil)))

;; --- sender --- ;;

;; TODO: Consider more proper package

(defun log-console (&key message)
  (send-log-console (get-frame-count) (incf-index-in-frame)
                    :message message))
