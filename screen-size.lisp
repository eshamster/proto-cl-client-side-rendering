(defpackage proto-cl-client-side-rendering/screen-size
  (:use :cl)
  (:export :update-screen-size
           :get-screen-size
           :set-screen-size)
  (:import-from :proto-cl-client-side-rendering/client-list-manager
                :get-new-client-id-list)
  (:import-from :proto-cl-client-side-rendering/frame-counter
                :get-frame-count
                :incf-index-in-frame)
  (:import-from :proto-cl-client-side-rendering/protocol
                :send-set-screen-size)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :*target-client-id-list*))
(in-package :proto-cl-client-side-rendering/screen-size)

;; Note: Assume that screen size is common for all clients.

;; --- data --- ;;

(defvar *screen-width* 800)
(defvar *screen-height* 600)

;; --- interface --- ;;

(defun update-screen-size ()
  (when (get-new-client-id-list)
    (let ((*target-client-id-list* (get-new-client-id-list)))
      (multiple-value-bind (width height) (get-screen-size)
        (set-screen-size :width width :height height)))))

(defun get-screen-size ()
  "Return screen size as multiple values of (width height)."
  (values *screen-width* *screen-height*))

(defun set-screen-size (&key width height)
  ;; FIXME: Validate arguments
  (setf *screen-width* width
        *screen-height* height)
  (send-set-screen-size (get-frame-count) (incf-index-in-frame)
                        :width width :height height))
