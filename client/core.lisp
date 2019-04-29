(defpackage proto-cl-client-side-rendering/client/core
  (:use :cl)
  (:export :output-client-js)
  ;; temporal
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
                :def-top-level-form.ps
                :enable-ps-experiment-syntax
                :with-use-ps-pack))
(in-package :proto-cl-client-side-rendering/client/core)

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

(defun.ps my-parse-bool (str)
  (number-to-bool (parse-int str)))

(defun.ps+ array-pair-to-hash (key-array value-array value-processor-array)
  (let ((result (make-hash-table)))
    (loop
       :for key :in key-array
       :for value :in value-array
       :for value-processor :in value-processor-array
       :do (setf (gethash key result) (funcall value-processor value)))
    result))

(defun.ps receiving-to-json (message)
  (let* ((split-message (message.split " "))
         (kind-code (parse-int (nth 0 split-message)))
         (kind (code-to-name kind-code))
         (frame (parse-int (nth 1 split-message)))
         (index-in-frame (parse-int (nth 2 split-message)))
         (body (nthcdr 3 split-message))
         (data (case kind
                 (:draw-circle
                  (array-pair-to-hash
                   (list :id :x :y :depth
                         :color :fill-p
                         :r)
                   body
                   (list #'parse-int #'parse-float #'parse-float #'parse-float
                         #'parse-int #'my-parse-bool
                         #'parse-float)))
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
                              (update-function (lambda (scene) nil)))
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
               (funcall update-function scene)))
      (render-loop))))

(defun.ps clear-scene (scene)
  (loop :while (> scene.children.length 0)
     :do (scene.remove (@ scene children 0))))

(defun.ps update-draw (scene)
  (let ((draw-commands (dequeue-draw-commands)))
    (when draw-commands
      (clear-scene scene)
      (dolist (command draw-commands)
        (let ((kind (code-to-name (gethash :kind command)))
              (data (gethash :data command)))
          (ecase kind
            (:draw-circle
             (let ((mesh (if (gethash :fill-p data)
                             (make-solid-circle :r (gethash :r data)
                                                :color (gethash :color data))
                             (make-wired-circle :r (gethash :r data)
                                                :color (gethash :color data)))))
               (mesh.position.set (gethash :x data)
                                  (gethash :y data)
                                  (gethash :depth data))
               (scene.add mesh)))))))))

(def-top-level-form.ps :run-start-2d-game
  (start-2d-game :screen-width 800 :screen-height 600
                 :rendered-dom (document.query-selector "#renderer")
                 :update-function #'update-draw))
