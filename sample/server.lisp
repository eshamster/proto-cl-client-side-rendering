(defpackage sample-proto-cl-client-side-rendering/server
  (:use :cl
        :cl-markup)
  (:export :start
           :stop
           :get-ningle-app
           :get-sample-list
           :get-current-sample-kind
           :get-port)
  (:import-from :sample-proto-cl-client-side-rendering/sample-basic
                :start-basic-sample
                :stop-basic-sample)
  (:import-from :sample-proto-cl-client-side-rendering/sample-screen-and-camera
                :start-screen-and-camera-sample
                :stop-screen-and-camera-sample)
  (:import-from :sample-proto-cl-client-side-rendering/sample-texture
                :start-texture
                :stop-texture)
  (:import-from :proto-cl-client-side-rendering
                :ensure-js-files
                :make-src-list-for-script-tag
                :make-client-side-rendering-middleware)
  (:import-from :alexandria
                :hash-table-keys))
(in-package :sample-proto-cl-client-side-rendering/server)

(defvar *server* nil)

(defvar *port* 5000)

(defun get-port ()
  *port*)

(defun start (&key (port *port*) (kind :basic))
  (stop)
  (setf *server*
        (clack:clackup
         (lack:builder
          (make-client-side-rendering-middleware
           :resource-root (merge-pathnames
                           "resource/"
                           (asdf:component-pathname
                            (asdf:find-system :sample-proto-cl-client-side-rendering))))
          *ningle-app*)
         :port port))
  (setf *port* port)
  (start-sample-game-loop :kind kind))

(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)
    (stop-sample-game-loop)))

;; --- static --- ;;

(defvar *ningle-app* (make-instance 'ningle:<app>))

(defun get-ningle-app ()
  *ningle-app*)

(setf (ningle:route *ningle-app* "/" :method :GET)
      (lambda (params)
        (declare (ignorable params))
        (ensure-js-files
         (merge-pathnames
          "js/" (asdf:system-source-directory :sample-proto-cl-client-side-rendering)))
        (with-output-to-string (str)
          (let ((cl-markup:*output-stream* str))
            (html5 (:head
                    (:title "A sample of client side rendering on Common Lisp")
                    (dolist (src (make-src-list-for-script-tag "js/"))
                      (markup (:script :src src nil))))
                   (:body
                    (:div (:textarea :id "js-code"
                                     :cols 80 :rows 10 :readonly t :disabled t nil))
                    (:div :id "renderer" nil)
                    (:script :src "js/client.js" nil)))))))

;; --- game loop --- ;;

(defvar *current-sample-kind* nil)

(defstruct sample-info start-proc stop-proc)

(defvar *sample-info-table* (make-hash-table))

(flet ((set-info (kind info)
         (setf (gethash kind *sample-info-table*) info)))
  (set-info :basic
            (make-sample-info :start-proc #'start-basic-sample
                              :stop-proc #'stop-basic-sample))
  (set-info :screen-and-camera
            (make-sample-info :start-proc #'start-screen-and-camera-sample
                              :stop-proc #'stop-screen-and-camera-sample))
  (set-info :texture
            (make-sample-info :start-proc #'start-texture
                              :stop-proc #'stop-texture)))

(defun get-sample-list ()
  (hash-table-keys *sample-info-table*))

(defun get-current-sample-kind ()
  *current-sample-kind*)

(defun must-get-sample-info (kind)
  (multiple-value-bind (info found-p)
      (gethash kind *sample-info-table*)
    (unless found-p
      (error "The kind \"~A\" is not valid. Valid kinds are ~A"
             kind (get-sample-list)))
    (check-type info sample-info)
    info))

(defun start-sample-game-loop (&key (kind :basic))
  (funcall (sample-info-start-proc
            (must-get-sample-info kind)))
  (setf *current-sample-kind* kind))

(defun stop-sample-game-loop ()
  (when *current-sample-kind*
    (funcall (sample-info-stop-proc
              (must-get-sample-info *current-sample-kind*)))
    (setf *current-sample-kind* nil)))
