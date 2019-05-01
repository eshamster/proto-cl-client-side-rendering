(defpackage proto-cl-client-side-rendering/client/socket
  (:use :cl)
  (:export :register-socket-on-message
           :send-json-to-server)
  (:import-from :parenscript
                :new
                :@)
  (:import-from :ps-experiment
                :defvar.ps
                :defun.ps
                :enable-ps-experiment-syntax))
(in-package :proto-cl-client-side-rendering/client/socket)

(enable-ps-experiment-syntax)

(defvar.ps *ws-socket*
    (new (#j.WebSocket# (+ "ws://" window.location.host "/ws"))))

(defun.ps register-socket-on-message (callback)
  (setf *ws-socket*.onmessage
        (lambda (e)
          (funcall callback e.data))))

(defun.ps send-json-to-server (json)
  (*ws-socket*.send ((@ #j.JSON# stringify) json)))
