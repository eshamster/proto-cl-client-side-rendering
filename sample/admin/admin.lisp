(defpackage sample-proto-cl-client-side-rendering/admin/admin
  (:use :cl
        :cl-markup)
  (:import-from :sample-proto-cl-client-side-rendering/server
                :start
                :get-ningle-app
                :get-sample-list
                :get-current-sample-kind
                :get-port)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :lack.response
                :response-headers
                :response-status)
  (:import-from :ningle
                :route
                :*response*)
  (:import-from :quri
                :url-encode))
(in-package :sample-proto-cl-client-side-rendering/admin/admin)

(defun get-param (param-name params)
  (cdr (assoc param-name params :test #'string=)))

(defun redirect (url)
  (setf (getf (response-headers *response*) :location) url)
  (setf (response-status *response*) 302)
  url)

(defun redirect-to-error (message)
  (redirect (format nil "/error/~D" (url-encode message))))

(progn
  (defun make-error-route ()
    (lambda (params)
      (setf (response-status *response*) 400)
      (with-output-to-string (str)
        (let ((cl-markup:*output-stream* str))
          (html5 (:head
                  (:title "Error"))
                 (:body
                  (:div (format nil "ERROR!!: ~A"
                                (cdr (assoc :message params))))))))))

  (setf (route (get-ningle-app) "/error/:message")
        (make-error-route)))

(progn
  (defun make-index-route ()
    (lambda (params)
      (declare (ignore params))
      (with-output-to-string (str)
        (let ((cl-markup:*output-stream* str)
              (current-sample (get-current-sample-kind)))
          (html5 (:head
                  (:title "Admin console for samples of cl-csr-2d-game"))
                 (:body
                  (:form
                   :action "/start" :method "POST"
                   (:select :name "sample"
                            :size (format nil "~D" (length (get-sample-list)))
                            (dolist (sample (sort (get-sample-list) #'string<))
                              (if (eq sample current-sample)
                                  (markup (:option :value sample :selected nil sample))
                                  (markup (:option :value sample sample)))))
                   (:input :type "submit" :value "Start/Restart"))))))))

  (setf (route (get-ningle-app) "/admin")
        (make-index-route)))

(progn
  (defun make-start-route ()
    (lambda (params)
      (block func
        (let ((sample-string (get-param "sample" params)))
          (unless sample-string
            (redirect-to-error "No sample name is specified.")
            (return-from func))
          (let ((sample (make-keyword sample-string)))
            (unless (find sample (get-sample-list))
              (redirect-to-error
               (format nil "The sample \"~A\" is not a valid name. One of ~D is expected."
                       sample (get-sample-list)))
              (return-from func))
            (start :kind sample)
            (redirect "/admin"))))))

  (setf (route (get-ningle-app) "/start" :method :post)
      (make-start-route)))
