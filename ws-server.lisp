(defpackage proto-cl-client-side-rendering/ws-server
  (:use :cl)
  (:export :*ws-app*
           :send-from-server
           :register-message-processor)
  (:import-from :jonathan
                :parse)
  (:import-from :websocket-driver
                :make-server
                :on
                :send
                :start-connection
                :ready-state))
(in-package :proto-cl-client-side-rendering/ws-server)

(defvar *server-instance-list* nil)

(defvar *message-processor-table* (make-hash-table))

(defun register-message-processor (id-as-symbol callback)
  (setf (gethash id-as-symbol *message-processor-table*) callback))

(defparameter *ws-app*
  (lambda (env)
    (let ((server (make-server env)))
      (push server *server-instance-list*)
      (on :message server
          (lambda (json-string)
            (let ((temp-table (parse json-string :as :hash-table))
                  (parsed-table (make-hash-table)))
              (maphash (lambda (key value)
                         (setf (gethash (intern (string-upcase key) "KEYWORD")
                                        parsed-table)
                               value))
                       temp-table)
              (maphash (lambda (id callback)
                         (declare (ignore id))
                         (let ((client-id 0)) ; TODO: (dummy)
                           (funcall callback client-id parsed-table)))
                       *message-processor-table*))))
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
