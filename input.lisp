(defpackage proto-cl-client-side-rendering/input
  (:use :cl)
  (:export :process-input-message)
  (:import-from :proto-cl-client-side-rendering/protocol
                :code-to-name)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :register-message-processor))
(in-package :proto-cl-client-side-rendering/input)

(defun process-input-message (message-table)
  (format t "Got: kind = ~A, key = ~A~%"
          (code-to-name (gethash :kind message-table))
          (gethash :key message-table)))

(register-message-processor 'input-processor #'process-input-message)
