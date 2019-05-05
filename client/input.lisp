(defpackage proto-cl-client-side-rendering/client/input
  (:use :cl)
  (:export :init-input)
  (:import-from :proto-cl-client-side-rendering/protocol
                :name-to-code)
  (:import-from :proto-cl-client-side-rendering/client/global
                :get-rendered-dom
                :get-screen-scale)
  (:import-from :proto-cl-client-side-rendering/client/socket
                :send-json-to-server)
  (:import-from :ps-experiment
                :defvar.ps
                :defun.ps
                :defun.ps+
                :enable-ps-experiment-syntax))
(in-package :proto-cl-client-side-rendering/client/input)

(enable-ps-experiment-syntax)

(defun.ps init-input ()
  (window.add-event-listener "keydown" on-keydown)
  (window.add-event-listener "keyup" on-keyup)
  (window.add-event-listener "mouseup" on-mouseup)
  (window.add-event-listener "mousedown" on-mousedown)
  (window.add-event-listener "mousemove" on-mousemove))

;; --- internal --- ;;

;; - keyboard - ;;

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
                                      :data (ps:create
                                             :key (adjust-key-name key))))
      (setf (gethash key *key-down-table*) t))))

(defun.ps on-keyup (e)
  (let ((key e.key))
    (send-json-to-server (ps:create :kind (name-to-code :key-up)
                                    :data (ps:create
                                           :key (adjust-key-name key))))
    (setf (gethash key *key-down-table*) nil)))

;; - mouse - ;;

(defun.ps calc-adjusted-input-point (x y)
  (let* ((renderer (get-rendered-dom))
         (canvas (renderer.query-selector "canvas"))
         (scale (get-screen-scale)))
    (values (floor (/ (- x renderer.offset-left) scale))
            (floor (/ (+ (- canvas.height y) renderer.offset-top) scale)))))

(defun.ps+ mouse-button-to-string (button)
  (case button
    (0 :left)
    (1 :center)
    (2 :rihgt)
    (t button)))

(defun.ps send-mouse-message (kind e)
  (multiple-value-bind (x y)
      (calc-adjusted-input-point e.client-x e.client-y))
  (send-json-to-server (ps:create :kind (name-to-code kind)
                                  :data (ps:create
                                         :button (mouse-button-to-string e.button)
                                         :x x
                                         :y y))))

(defun.ps+ on-mousedown (e)
  (send-mouse-message :mouse-down e))

(defun.ps on-mouseup (e)
  (send-mouse-message :mouse-up e))

(defun.ps on-mousemove (e)
  (send-mouse-message :mouse-move e))
