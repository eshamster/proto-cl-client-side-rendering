(defpackage proto-cl-client-side-rendering/camera
  (:use :cl)
  (:export :update-camera-info
           :get-camera-center-pos
           :get-camera-scale
           :set-camera-center-pos
           :set-camera-scale)
  (:import-from :proto-cl-client-side-rendering/client-list-manager
                :get-new-client-id-list
                :get-deleted-client-id-list)
  (:import-from :proto-cl-client-side-rendering/frame-counter
                :get-frame-count
                :incf-index-in-frame)
  (:import-from :proto-cl-client-side-rendering/protocol
                :send-set-camera)
  (:import-from :proto-cl-client-side-rendering/screen-size
                :get-screen-size)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :*target-client-id-list*))
(in-package :proto-cl-client-side-rendering/camera)

;; --- data --- ;;

(defstruct camera-info center-x center-y scale)

(defvar *camera-info-table* (make-hash-table))

;; --- interface --- ;;

(defun update-camera-info ()
  (dolist (new-client (get-new-client-id-list))
    (setf (gethash new-client *camera-info-table*) (init-camera-info)))
  (dolist (deleted-client (get-deleted-client-id-list))
    (remhash deleted-client *camera-info-table*)))

(defun get-camera-center-pos (client-id)
  (let ((info (get-camera-info client-id)))
    (values (camera-info-center-x info)
            (camera-info-center-y info))))

(defun set-camera-center-pos (client-id center-x center-y)
  (let ((info (get-camera-info client-id)))
    (setf (camera-info-center-x info) center-x
          (camera-info-center-y info) center-y)
    (send-camera-info client-id)))

(defun get-camera-scale (client-id)
  (let ((info (get-camera-info client-id)))
    (camera-info-scale info)))

(defun set-camera-scale (client-id scale)
  (let ((info (get-camera-info client-id)))
    (setf (camera-info-scale info) scale)
    (send-camera-info client-id)))

;; --- internal --- ;;

(defun init-camera-info ()
  (multiple-value-bind (width height) (get-screen-size)
    (make-camera-info :center-x (* width 1/2)
                      :center-y (* height 1/2)
                      :scale 1)))

(defun get-camera-info (client-id)
  (gethash client-id *camera-info-table*))

(defun send-camera-info (client-id)
  (let ((info (get-camera-info client-id))
        (*target-client-id-list* (list client-id)))
    (send-set-camera (get-frame-count) (incf-index-in-frame)
                     :center-x (camera-info-center-x info)
                     :center-y (camera-info-center-y info)
                     :scale (camera-info-scale info))))
