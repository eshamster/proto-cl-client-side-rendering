(defpackage proto-cl-client-side-rendering/client/message
  (:use :cl)
  (:export :dequeue-draw-commands
           :interpret-draw-command
           :process-message)
  (:import-from :proto-cl-client-side-rendering/client/graphics
                :make-solid-circle
                :make-wired-circle)
  (:import-from :proto-cl-client-side-rendering/protocol
                :code-to-name
                :name-to-code
                :draw-code-p
                :number-to-bool)
  (:import-from :parenscript
                :chain
                :new
                :@)
  (:import-from :ps-experiment
                :defvar.ps
                :defvar.ps+
                :defun.ps
                :defun.ps+
                :enable-ps-experiment-syntax))
(in-package :proto-cl-client-side-rendering/client/message)

(enable-ps-experiment-syntax)

(defvar.ps+ *frame-json-cache* (list)) ; per frame

(defvar.ps+ *draw-command-buffer* (list)) ; per frame
(defvar.ps+ *draw-command-queue* (list))  ; frames

(defun.ps push-draw-command-to-buffer (parsed-message)
  (*draw-command-buffer*.push parsed-message))

(defun.ps queue-draw-commands-in-buffer ()
  (*draw-command-queue*.unshift *draw-command-buffer*)
  (setf *draw-command-buffer* (list)))

(defun.ps dequeue-draw-commands ()
  (*draw-command-queue*.pop))

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
")
          (when (draw-code-p (gethash :kind parsed))
            (push-draw-command-to-buffer parsed))))
      (queue-draw-commands-in-buffer)
      (setf *frame-json-cache* (list)))))

(defun.ps+ target-kind-p (kind parsed-message)
  (eq (gethash :kind parsed-message)
      (name-to-code kind)))

(defun.ps receiving-to-json (message)
  (#j.JSON.parse# message))

;; TODO: The following should move to another package

(defun.ps interpret-draw-command (scene command)
  (let* ((kind (code-to-name (gethash :kind command)))
         (data (gethash :data command))
         (mesh (ecase kind
                 (:draw-circle
                  (if (number-to-bool (gethash :fill-p data))
                      (make-solid-circle :r (gethash :r data)
                                         :color (gethash :color data))
                      (make-wired-circle :r (gethash :r data)
                                         :color (gethash :color data)))))))
    (mesh.position.set (gethash :x data)
                       (gethash :y data)
                       (gethash :depth data))
    (scene.add mesh)))
