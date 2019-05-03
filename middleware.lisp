(defpackage proto-cl-client-side-rendering/middleware
  (:use :cl)
  (:export :make-client-side-rendering-middleware)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :*ws-app*)
  (:import-from :proto-cl-client-side-rendering/client/core
                :output-client-js))
(in-package :proto-cl-client-side-rendering/middleware)

(defun make-client-side-rendering-middleware (&key resource-root)
  (lambda (app)
    (lambda (env)
      (output-client-js (merge-pathnames "js/client.js" resource-root))
      (let ((uri (getf env :request-uri)))
        (if (string= uri "/ws")
            (funcall *ws-app* env)
            (funcall (make-static-middleware
                      app :resource-root resource-root)
                     env))))))

(defun make-static-middleware (app &key resource-root)
  (funcall lack.middleware.static:*lack-middleware-static*
           app
           :path (lambda (path)
                    (if (ppcre:scan "^(?:/js/)" path)
                        path
                        nil))
           :root resource-root))
