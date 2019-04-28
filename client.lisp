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
         (kind-code (parse-int (nth 0 split-message)))
         (kind (code-to-name kind-code))
         (frame (parse-int (nth 1 split-message)))
         (index-in-frame (parse-int (nth 2 split-message)))
         (body (nthcdr 3 split-message))
         (data (case kind
                 (t (ps:create :message body)))))
    (ps:create :kind kind-code
               :frame frame
               :no index-in-frame
               :data data)))

;; --- graphic --- ;;

;; The followings are based on cl-web-2d-game

(defun.ps init-camera (offset-x offset-y width height)
  (let* ((x offset-x)
         (y offset-y)
         (z 1000)
         (camera (new (#j.THREE.OrthographicCamera#
                       (* x -1) (- width x)
                       (- height y) (* y -1)
                       0 (* z 2)))))
    (camera.position.set 0 0 z)
    camera))

(defun.ps initialize-screen-size (rendered-dom renderer screen-width screen-height resize-to-screen-p)
  (setf *resize-to-screen-p* resize-to-screen-p)
  (labels ((calc-scale ()
             (min (/ window.inner-width screen-width)
                  (/ (- window.inner-height *window-height-adjust*) screen-height)))
           (set-position-by-size (width height)
             (setf rendered-dom.style.position "absolute"
                   rendered-dom.style.left (+ (/ (- window.inner-width width) 2) "px")
                   rendered-dom.style.top (+ (/ (- window.inner-height height) 2) "px")))
           (set-size (width height)
             (renderer.set-size width height)
             (set-position-by-size width height))
           (resize ()
             (let ((scale (if *resize-to-screen-p* (calc-scale) 1)))
               (set-size (* screen-width scale)
                         (* screen-height scale)))))
    (resize)
    (let ((resize-timer nil))
      (window.add-event-listener
       "resize" (lambda (e)
                  (declare (ignore e))
                  (when resize-timer
                    (clear-timeout resize-timer))
                  (setf resize-timer
                        (set-timeout (lambda () (resize))
                                     100)))))))

(defun.ps start-2d-game (&key screen-width screen-height
                              rendered-dom
                              (resize-to-screen-p nil)
                              (init-function (lambda (scene) nil))
                              (update-function (lambda () nil)))
  (let* ((scene (new (#j.THREE.Scene#)))
         (renderer (new #j.THREE.WebGLRenderer#))
         (camera (init-camera 0 0 screen-width screen-height)))
    (initialize-screen-size rendered-dom renderer
                            screen-width screen-height
                            resize-to-screen-p)
    (chain rendered-dom
           (append-child renderer.dom-element))
    (let ((light (new (#j.THREE.DirectionalLight# 0xffffff))))
      (light.position.set 0 0.7 0.7)
      (scene.add light))
    (funcall init-function scene)
    (labels ((render-loop ()
               (request-animation-frame render-loop)
               (renderer.render scene camera)
               (funcall update-function)))
      (render-loop))))

(def-top-level-form.ps :run-start-2d-game
  (start-2d-game :screen-width 800 :screen-height 600
                 :rendered-dom (document.query-selector "#renderer")))
