(defpackage proto-cl-client-side-rendering/middleware
  (:use :cl)
  (:export :make-hot-load-middleware)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :*ws-app*)
  (:import-from :proto-cl-client-side-rendering/client
                :output-client-js))
(in-package :proto-cl-client-side-rendering/middleware)

(defun make-hot-load-middleware (&key main-js-path string-url)
  (lambda (app)
    (lambda (env)
      ;; temp
      (output-client-js (merge-pathnames "client.js" main-js-path))
      (let ((uri (getf env :request-uri)))
        (if (string= uri string-url)
            (funcall *ws-app* env)
            (funcall app env))))))
