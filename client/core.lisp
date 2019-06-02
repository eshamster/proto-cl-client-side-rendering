(defpackage proto-cl-client-side-rendering/client/core
  (:use :cl)
  (:export :output-client-js)
  (:import-from :proto-cl-client-side-rendering/client/camera
                :init-camera)
  (:import-from :proto-cl-client-side-rendering/client/input
                :init-input)
  (:import-from :proto-cl-client-side-rendering/client/message
                :dequeue-draw-commands
                :interpret-draw-command
                :process-message)
  (:import-from :proto-cl-client-side-rendering/client/renderer
                :init-screen-size
                :get-screen-size)
  (:import-from :proto-cl-client-side-rendering/client/socket
                :register-socket-on-message)
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
(in-package :proto-cl-client-side-rendering/client/core)

(enable-ps-experiment-syntax)

;; --- compiler -- - ;;

(defun output-client-js (file-path)
  (with-open-file (file file-path
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (princ (with-use-ps-pack (:this)
             (register-socket-on-message #'process-message)
             (init-input))
           file)))

;; --- initializer --- ;;

(defun.ps start-2d-game (&key rendered-dom
                              (resize-to-screen-p t)
                              (init-function (lambda (scene) nil))
                              (update-function (lambda (scene) nil)))
  (let* ((scene (new (#j.THREE.Scene#)))
         (renderer (new #j.THREE.WebGLRenderer#))
         camera)
    (init-screen-size rendered-dom renderer resize-to-screen-p)
    (multiple-value-bind (screen-width screen-height) (get-screen-size)
      (setf camera (init-camera 0 0 screen-width screen-height)))
    (chain rendered-dom
           (append-child renderer.dom-element))
    (let ((light (new (#j.THREE.DirectionalLight# 0xffffff))))
      (light.position.set 0 0.7 0.7)
      (scene.add light))
    (funcall init-function scene)
    (labels ((render-loop ()
               (request-animation-frame render-loop)
               (renderer.render scene camera)
               (funcall update-function scene)))
      (render-loop))))

(defun.ps clear-scene (scene)
  (loop :while (> scene.children.length 0)
     :do (scene.remove (@ scene children 0))))

(defun.ps+ update-draw (scene)
  (let ((draw-commands (dequeue-draw-commands)))
    (when draw-commands
      (dolist (command draw-commands)
        (interpret-draw-command scene command)))))

(def-top-level-form.ps :run-start-2d-game
  (start-2d-game :rendered-dom (document.query-selector "#renderer")
                 :update-function #'update-draw))
