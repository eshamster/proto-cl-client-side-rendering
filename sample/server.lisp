(defpackage sample-proto-cl-client-side-rendering/server
  (:use :cl
        :cl-markup)
  (:export :start
           :stop)
  (:import-from :sample-proto-cl-client-side-rendering/game-loop
                :start-sample-game-loop
                :stop-sample-game-loop)
  (:import-from :proto-cl-client-side-rendering
                :ensure-js-files
                :make-src-list-for-script-tag
                :make-client-side-rendering-middleware))
(in-package :sample-proto-cl-client-side-rendering/server)

(defvar *server* nil)

(defun start (&key (port 5000))
  (stop)
  (start-sample-game-loop)
  (setf *server*
        (clack:clackup
         (lack:builder
          (make-client-side-rendering-middleware
           :client-js-path (merge-pathnames
                            "js/client.js"
                            (asdf:component-pathname
                             (asdf:find-system :proto-cl-client-side-rendering)))
           :string-url "/ws")
          *static-app*)
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

(defvar *static-app*
  (lack:builder
   (:static :path (lambda (path)
                    (if (ppcre:scan "^(?:/js/)" path)
                        path
                        nil))
            :root (asdf:component-pathname
                   (asdf:find-system :proto-cl-client-side-rendering)))
   *ningle-app*))
