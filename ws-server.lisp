(defpackage proto-cl-client-side-rendering/ws-server
  (:use :cl)
  (:export :*ws-app*
           :send-from-server)
  (:import-from :proto-cl-client-side-rendering/compiler
                :compile-ps-string)
  (:import-from :websocket-driver
                :make-server
                :on
                :send
                :start-connection
                :ready-state))
(in-package :proto-cl-client-side-rendering/ws-server)

;; --- server --- ;;

(defvar *server-instance-list* nil)

(defparameter *ws-app*
  (lambda (env)
    (let ((server (make-server env)))
      (push server *server-instance-list*)
      (on :message server
          (lambda (ps-code)
            (format t "~&Server got: ~A~%" ps-code)
            (send-from-server ps-code)))
      (lambda (responder)
        (declare (ignore responder))
        (format t "~&Server connected")
        (start-connection server)))))

(defun send-from-server (message)
  (dolist (server (copy-list *server-instance-list*))
    (case (ready-state server)
      (:open (send server message))
      (:closed (print "Connecction closed")
               (setf *server-instance-list* (remove server *server-instance-list*)))
      ;; otherwise do nothing
      )))
