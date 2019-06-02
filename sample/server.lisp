(defpackage sample-proto-cl-client-side-rendering/server
  (:use :cl
        :cl-markup)
  (:export :start
           :stop)
  (:import-from :sample-proto-cl-client-side-rendering/sample-basic
                :start-basic-sample
                :stop-basic-sample)
  (:import-from :sample-proto-cl-client-side-rendering/sample-screen-and-camera
                :start-screen-and-camera-sample
                :stop-screen-and-camera-sample)
  (:import-from :proto-cl-client-side-rendering
                :ensure-js-files
                :make-src-list-for-script-tag
                :make-client-side-rendering-middleware))
(in-package :sample-proto-cl-client-side-rendering/server)

(defvar *server* nil)

(defun start (&key (port 5000) (kind :basic))
  (stop)
  (start-sample-game-loop :kind kind)
  (setf *server*
        (clack:clackup
         (lack:builder
          (make-client-side-rendering-middleware
           :resource-root (merge-pathnames
                           "resource/"
                           (asdf:component-pathname
                            (asdf:find-system :sample-proto-cl-client-side-rendering))))
          *ningle-app*)
         :port port)))

(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)
    (stop-sample-game-loop)))

;; --- static --- ;;

(defvar *ningle-app* (make-instance 'ningle:<app>))

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

(defun start-sample-game-loop (&key (kind :basic))
  (ecase kind
    (:basic (start-basic-sample))
    (:screen-and-camera (start-screen-and-camera-sample)))
  (setf *current-sample-kind* kind))

(defun stop-sample-game-loop ()
  (when *current-sample-kind*
    (ecase *current-sample-kind*
      (:basic (stop-basic-sample))
      (:screen-and-camera (stop-screen-and-camera-sample)))
    (setf *current-sample-kind* nil)))
