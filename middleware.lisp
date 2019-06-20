(defpackage proto-cl-client-side-rendering/middleware
  (:use :cl)
  (:export :make-client-side-rendering-middleware)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :*ws-app*)
  (:import-from :proto-cl-client-side-rendering/client/core
                :output-client-js)
  (:import-from :proto-cl-client-side-rendering/texture
                :set-image-root-path)
  (:import-from :proto-cl-client-side-rendering/utils
                :ensure-js-files))
(in-package :proto-cl-client-side-rendering/middleware)

(defun make-client-side-rendering-middleware (&key
                                                resource-root
                                                (image-relarive-path "img/"))
  (ensure-js-files  (merge-pathnames "js/" resource-root))
  (set-image-root-path (merge-pathnames image-relarive-path resource-root))
  (lambda (app)
    (lambda (env)
      (output-client-js (merge-pathnames "js/client.js" resource-root))
      (let ((uri (getf env :request-uri)))
        (if (string= uri "/ws")
            (funcall *ws-app* env)
            (funcall (make-static-middleware
                      app
                      :resource-root resource-root
                      :image-relarive-path image-relarive-path)
                     env))))))

(defun make-static-middleware (app &key
                                     resource-root
                                     image-relarive-path)
  (funcall lack.middleware.static:*lack-middleware-static*
           app
           :path (lambda (path)
                   (if (ppcre:scan (format nil "^(?:/js/|/~A)" image-relarive-path)
                                   path)
                        path
                        nil))
           :root resource-root))
