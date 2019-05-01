(defpackage proto-cl-client-side-rendering/client/input
  (:use :cl)
  (:export :init-input)
  (:import-from :proto-cl-client-side-rendering/protocol
                :name-to-code)
  (:import-from :proto-cl-client-side-rendering/client/socket
                :send-json-to-server)
  (:import-from :ps-experiment
                :defvar.ps
                :defun.ps
                :enable-ps-experiment-syntax))
(in-package :proto-cl-client-side-rendering/client/input)

(enable-ps-experiment-syntax)

(defun.ps init-input ()
  (window.add-event-listener "keydown" on-keydown)
  (window.add-event-listener "keyup" on-keyup))

;; --- internal --- ;;

(defvar.ps *key-down-table* (make-hash-table))

(defun.ps adjust-key-name (key-name)
  (cond ((key-name.starts-with "Arrow")
         (key-name.substr (length "Arrow")))
        ((string= key-name " ") :space)
        (t key-name)))

(defun.ps on-keydown (e)
  (let ((key e.key))
    (unless (gethash key *key-down-table*)
      (send-json-to-server (ps:create :kind (name-to-code :key-down)
                                      :key (adjust-key-name key)))
      (setf (gethash key *key-down-table*) t))))

(defun.ps on-keyup (e)
  (let ((key e.key))
    (send-json-to-server (ps:create :kind (name-to-code :key-up)
                                    :key (adjust-key-name key)))
    (setf (gethash key *key-down-table*) nil)))
