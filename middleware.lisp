(defpackage proto-cl-client-side-rendering/middleware
  (:use :cl)
  (:export :make-client-side-rendering-middleware)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :*ws-app*)
  (:import-from :proto-cl-client-side-rendering/client/core
                :output-client-js))
(in-package :proto-cl-client-side-rendering/middleware)

(defun make-client-side-rendering-middleware (&key client-js-path web-socket-url)
  (lambda (app)
    (lambda (env)
      (output-client-js client-js-path)
      (let ((uri (getf env :request-uri)))
        (if (string= uri web-socket-url)
            (funcall *ws-app* env)
            (funcall app env))))))
