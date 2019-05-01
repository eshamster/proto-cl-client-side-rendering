(defpackage proto-cl-client-side-rendering/ws-server
  (:use :cl)
  (:export :*ws-app*
           :send-from-server
           :register-message-processor
           :get-client-id-list)
  (:import-from :jonathan
                :parse)
  (:import-from :websocket-driver
                :make-server
                :on
                :send
                :start-connection
                :ready-state))
(in-package :proto-cl-client-side-rendering/ws-server)

(defvar *latest-client-id* 0)

(defstruct client-info
  target-server
  (id (incf *latest-client-id*)))

(defvar *client-info-list* nil)

(defun get-client-id-list ()
  (sort (loop :for info :in *client-info-list*
           :collect (client-info-id info))
        #'<))

(defvar *message-processor-table* (make-hash-table))

(defun register-message-processor (id-as-symbol callback)
  (setf (gethash id-as-symbol *message-processor-table*) callback))

(defparameter *ws-app*
  (lambda (env)
    (let* ((server (make-server env))
           (client-info (make-client-info :target-server server))
           (client-id (client-info-id client-info)))
      (push client-info *client-info-list*)
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
                         (funcall callback client-id parsed-table))
                       *message-processor-table*))))
      (lambda (responder)
        (declare (ignore responder))
        (format t "~&Connection started: ~D" client-id)
        (start-connection server)))))

(defun send-from-server (message)
  (dolist (client-info (copy-list *client-info-list*))
    (let ((server (client-info-target-server client-info)))
      (case (ready-state server)
        (:open (send server message))
        (:closed (format t "~&Connection closed: ~D" (client-info-id client-info))
                 (setf *client-info-list* (remove client-info *client-info-list*)))
        ;; otherwise do nothing
        ))))
