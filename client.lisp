(defpackage proto-cl-client-side-rendering/client
  (:use :cl)
  (:export :output-client-js)
  (:import-from :parenscript
                :chain
                :new)
  (:import-from :ps-experiment
                :defvar.ps
                :defun.ps
                :def-top-level-form.ps
                :enable-ps-experiment-syntax
                :with-use-ps-pack))
(in-package :proto-cl-client-side-rendering/client)

(enable-ps-experiment-syntax)

(defvar.ps ws-socket
    (new (#j.WebSocket# (+ "ws://" window.location.host "/ws"))))

(def-top-level-form.ps register-on-message
  ;; TODO: Decode unicode
  (setf ws-socket.onmessage
        (lambda (e)
          (setf (chain document (get-element-by-id "js-code") value) e.data)
          (eval.call window e.data))))

#|
;; Currently not used but remained for reference to send info to server.
(defun.ps send-ps-code ()
  (ws-socket.send (ps-code-value)))
|#

(defun output-client-js (file-path)
  (with-open-file (file file-path
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (princ (with-use-ps-pack (:this))
           file)))
