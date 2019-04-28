(defpackage proto-cl-client-side-rendering/protocol
  (:use :cl)
  (:export :code-to-name
           :name-to-code
           :send-frame-start
           :send-frame-end)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :send-from-server)
  (:import-from :ps-experiment
                :defvar.ps+
                :defun.ps+
                :def-top-level-form.ps+))
(in-package :proto-cl-client-side-rendering/protocol)

(defvar.ps+ *code-to-name-table* nil)
(defvar.ps+ *name-to-code-table* nil)

(defun.ps+ initialize-table ()
  (setf *code-to-name-table* (make-hash-table)
        *name-to-code-table* (make-hash-table))
  (dolist (pair '((0 :frame-start)
                  (1 :frame-end)
                  (11 :draw-rect)
                  (12 :draw-circle)))
    (let ((code (car pair))
          (name (cadr pair)))
      (setf (gethash code *code-to-name-table*) name
            (gethash name *name-to-code-table*) code))))

(def-top-level-form.ps+ :call-protocol-initializer
  (initialize-table))

(defun.ps+ code-to-name (code)
  (gethash code *code-to-name-table*))

(defun.ps+ name-to-code (name)
  (gethash name *name-to-code-table*))

;; --- sender --- ;;

(defun send-frame-start (frame index-in-frame)
  (send-from-server (format nil "~D ~D ~D"
                            (name-to-code :frame-start)
                            frame index-in-frame)))

(defun send-frame-end (frame index-in-frame)
  (send-from-server (format nil "~D ~D ~D"
                            (name-to-code :frame-end)
                            frame index-in-frame)))
