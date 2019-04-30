(defpackage proto-cl-client-side-rendering/server
  (:use :cl)
  (:export :start
           :stop
           :server-started-p)
  (:import-from :proto-cl-client-side-rendering/static-server
                :*static-app*)
  (:import-from :proto-cl-client-side-rendering/middleware
                :make-hot-load-middleware))
(in-package :proto-cl-client-side-rendering/server)

(defvar *server* nil)
(defvar *port* nil)

(defun server-started-p ()
  (not (null *server*)))

(defun start (&key (port 5000))
  (stop)
  (setf *port* port
        *server*
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
    (setf *server* nil
          *port* nil)))
