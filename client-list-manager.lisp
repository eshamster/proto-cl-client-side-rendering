(defpackage proto-cl-client-side-rendering/client-list-manager
  (:use :cl)
  (:export :update-client-list
           :get-new-client-id-list
           :get-deleted-client-id-list
           :get-client-id-list
           :client-alive-p
           :with-sending-to-new-clients)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :*target-client-id-list*
                :register-callback-on-connecting
                :register-callback-on-disconnecting))
(in-package :proto-cl-client-side-rendering/client-list-manager)

;; TODO: should lock buffers

;; --- registration --- ;;

(register-callback-on-connecting
 'detect-new-client (lambda (client-id)
                      (pushnew client-id *new-client-list-buffer*)))

(register-callback-on-disconnecting
 'detect-deleted-client (lambda (client-id)
                          (pushnew client-id *deleted-client-list-buffer*)))

;; --- interface --- ;;

(defun update-client-list ()
  (setf *new-client-list* *new-client-list-buffer*
        *new-client-list-buffer* nil
        *deleted-client-list* *deleted-client-list-buffer*
        *deleted-client-list-buffer* nil)
  (setf *client-list* (append *client-list* *new-client-list*))
  (dolist (deleted-id *deleted-client-list*)
    (setf *client-list* (delete deleted-id *client-list*))))

(defun get-new-client-id-list ()
  *new-client-list*)

(defun get-deleted-client-id-list ()
  *deleted-client-list*)

(defun get-client-id-list ()
  *client-list*)

(defun client-alive-p (client-id)
  (find client-id *client-list*))

(defmacro with-sending-to-new-clients (() &body body)
  (let ((new-clients (gensym)))
    `(let ((,new-clients (get-new-client-id-list)))
       (when ,new-clients
         (let ((*target-client-id-list* ,new-clients))
           ,@body)))))

;; --- internal --- ;;


(defvar *new-client-list* nil)
(defvar *new-client-list-buffer* nil)

(defvar *deleted-client-list* nil)
(defvar *deleted-client-list-buffer* nil)

(defvar *client-list* nil)

(register-callback-on-connecting
 'add-new-client-list-to-buffer
 (lambda (client-id) (push client-id *new-client-list-buffer*)))
