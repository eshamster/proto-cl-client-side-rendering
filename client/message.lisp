(defpackage proto-cl-client-side-rendering/client/message
  (:use :cl)
  (:export :dequeue-draw-commands
           :interpret-draw-command
           :process-message)
  (:import-from :proto-cl-client-side-rendering/client/graphics
                :make-solid-rect
                :make-wired-rect
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

;; debug
(defun.ps print-message-stat (message-stat)
  (let ((total 0)
        (text ""))
    (dolist (key (sort (-object.keys message-stat) (lambda (a b) (< a b))))
      (let ((count (gethash key message-stat)))
        (incf text (+ (code-to-name key) ":" #\Tab
                      count #\Newline))
        (incf total count)))
    (setf text
          (+ "TOTAL: " total #\Newline "---" #\Newline text))
    (setf (chain document (get-element-by-id "js-code") value) text)))

(defun.ps+ process-message (message)
  (let ((parsed-message (receiving-to-json message)))
    (push-message-to-cach parsed-message)
    (when (target-kind-p :frame-end parsed-message)
      (let ((message-stat (make-hash-table)))
        (dolist (parsed *frame-json-cache*)
          (let ((kind-code (gethash :kind parsed)))
            (symbol-macrolet ((count (gethash kind-code message-stat)))
              (unless count
                (setf count 0))
              (incf count))
            (cond ((eq (code-to-name kind-code) :log-console)
                   (interpret-log-console parsed))
                  ((draw-code-p kind-code)
                   (push-draw-command-to-buffer parsed)))))
        (print-message-stat message-stat))
      (queue-draw-commands-in-buffer)
      (setf *frame-json-cache* (list)))))

(defun.ps+ target-kind-p (kind parsed-message)
  (eq (gethash :kind parsed-message)
      (name-to-code kind)))

(defun.ps receiving-to-json (message)
  (#j.JSON.parse# message))

(defun.ps interpret-log-console (command)
  (console.log (@ command :data :message)))

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
                                         :color (gethash :color data))))
                 (:draw-rect
                  (if (number-to-bool (gethash :fill-p data))
                      (make-solid-rect :width (gethash :width data)
                                       :height (gethash :height data)
                                       :color (gethash :color data))
                      (make-wired-rect :width (gethash :width data)
                                       :height (gethash :height data)
                                       :color (gethash :color data)))))))
    (mesh.position.set (gethash :x data)
                       (gethash :y data)
                       (gethash :depth data))
    (let ((rotate (gethash :rotate data)))
      (when rotate
        (setf mesh.rotation.z rotate)))
    (scene.add mesh)))
