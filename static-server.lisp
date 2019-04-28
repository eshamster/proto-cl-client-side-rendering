(defpackage proto-cl-client-side-rendering/static-server
  (:use :cl
        :cl-markup)
  (:export *static-app*)
  (:import-from :proto-cl-client-side-rendering/utils
                :ensure-js-files
                :make-src-list-for-script-tag))
(in-package :proto-cl-client-side-rendering/static-server)

(defvar *ningle-app* (make-instance 'ningle:<app>))

(setf (ningle:route *ningle-app* "/" :method :GET)
      (lambda (params)
        (declare (ignorable params))
        (ensure-js-files
         (merge-pathnames "js/"
                          (asdf:system-source-directory :proto-cl-client-side-rendering)))
        (with-output-to-string (str)
          (let ((cl-markup:*output-stream* str))
            (html5 (:head
                    (:title "A sample of WebSocket on Common Lisp")
                    (dolist (src (make-src-list-for-script-tag "js/"))
                      (markup (:script :src src nil)))
                    (:script :src "js/main.js" nil))
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
