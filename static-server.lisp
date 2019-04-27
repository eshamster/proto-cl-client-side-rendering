(defpackage proto-cl-client-side-rendering/static-server
  (:use :cl
        :cl-markup)
  (:export *static-app*))
(in-package :proto-cl-client-side-rendering/static-server)

(defvar *ningle-app* (make-instance 'ningle:<app>))

(setf (ningle:route *ningle-app* "/" :method :GET)
      (lambda (params)
        (declare (ignorable params))
        (with-output-to-string (str)
          (let ((cl-markup:*output-stream* str))
            (html5 (:head
                    (:title "A sample of WebSocket on Common Lisp")
                    (:script :src "js/client.js" nil)
                    (:script :src "js/main.js" nil))
                   (:body
                    (:div (:textarea :id "js-code"
                                     :cols 80 :rows 10 :readonly t :disabled t nil))))))))

(defvar *static-app*
  (lack:builder
   (:static :path (lambda (path)
                    (if (ppcre:scan "^(?:/js/)" path)
                        path
                        nil))
            :root (asdf:component-pathname
                   (asdf:find-system :proto-cl-client-side-rendering)))
   *ningle-app*))
