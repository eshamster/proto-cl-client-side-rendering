(defpackage proto-cl-client-side-rendering/client
  (:use :cl)
  (:export :output-client-js)
  (:import-from :proto-cl-client-side-rendering/protocol
                :code-to-name
                :name-to-code)
  (:import-from :parenscript
                :chain
                :new
                :@)
  (:import-from :ps-experiment
                :defvar.ps
                :defvar.ps+
                :defun.ps
                :defun.ps+
                :def-top-level-form.ps
                :enable-ps-experiment-syntax
                :with-use-ps-pack))
(in-package :proto-cl-client-side-rendering/client)

(enable-ps-experiment-syntax)

(defvar.ps ws-socket
    (new (#j.WebSocket# (+ "ws://" window.location.host "/ws"))))

(def-top-level-form.ps register-on-message
  (setf ws-socket.onmessage
        (lambda (e)
          (process-message e.data))))

#|
;; Currently not used but remained for reference to send info to server.
(defun.ps send-ps-code ()
  (ws-socket.send (ps-code-value)))
|#

;; --- compiler -- - ;;

(defun output-client-js (file-path)
  (with-open-file (file file-path
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (princ (with-use-ps-pack (:this))
           file)))

;; --- process message --- ;;

(defvar.ps+ *frame-json-cache* (list))

(defun.ps push-message-to-cach (parsed-message)
  (*frame-json-cache*.push parsed-message))

(defun.ps process-message (message)
  (let ((parsed-message (receiving-to-json message)))
    (push-message-to-cach parsed-message)
    (when (target-kind-p :frame-end parsed-message)
      (symbol-macrolet ((value (chain document (get-element-by-id "js-code") value)))
        (setf value "")
        (dolist (parsed *frame-json-cache*)
          (incf value ((@ #j.JSON# stringify) parsed))
          (incf value "
")))
      (setf *frame-json-cache* (list)))))

(defun.ps+ target-kind-p (kind parsed-message)
  (eq (gethash :kind parsed-message)
      (name-to-code kind)))

(defun.ps receiving-to-json (message)
  (let* ((split-message (message.split " "))
         (kind-code (parse-int (car split-message)))
         (kind (code-to-name kind-code))
         (body (cdr split-message))
         (data (case kind
                 ((:frame-start :frame-end)
                  (ps:create :frame (parse-int (car body))
                             :no (parse-int (cadr body))))
                 (t (ps:create :message message)))))
    (ps:create :kind kind-code :data data)))
