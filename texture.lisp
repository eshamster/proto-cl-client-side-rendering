(defpackage proto-cl-client-side-rendering/texture
  (:use :cl)
  (:export :update-texture
           :make-image-uv
           :load-image
           :get-image-size
           :get-image-id
           :set-image-path)
  (:import-from :proto-cl-client-side-rendering/client-list-manager
                :get-new-client-id-list)
  (:import-from :proto-cl-client-side-rendering/frame-counter
                :get-frame-count
                :incf-index-in-frame)
  (:import-from :proto-cl-client-side-rendering/protocol
                :send-load-texture
                :send-load-image)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :*target-client-id-list*)
  (:import-from :alexandria
                :ensure-gethash
                :maphash-values)
  (:import-from :opticl
                :read-png-file
                :with-image-bounds))
(in-package :proto-cl-client-side-rendering/texture)

;; --- data --- ;;

(defvar *texture-id* 0)
(defvar *image-id* 0)

(defvar *image-root-path* nil)
(defvar *image-relative-path* nil)

(defvar *texture-table* (make-hash-table :test 'equal)
  "Key: A relative path from image root; Value: texture-info")
(defvar *image-table* (make-hash-table)
  "Key: A name represented as a keyword; value: image-info")

(defstruct texture-info
  (id (incf *texture-id*))
  path
  alpha-path
  width
  height)

(defstruct image-uv
  (x 0)
  (y 0)
  (width 1)
  (height 1))

(defstruct image-info
  (id (incf *image-id*))
  texture-id
  (uv (make-image-uv)))

;; --- interface --- ;;

(defun update-texture ()
  (let ((new-clients (get-new-client-id-list)))
    (when new-clients
      (let ((*target-client-id-list* new-clients))
        (maphash-values (lambda (tex-info)
                          (process-load-texture tex-info))
                        *texture-table*)
        (maphash-values (lambda (img-info)
                          (process-load-image img-info))
                        *image-table*)))))

(defun load-image (&key path alpha-path name (uv (make-image-uv)))
  "Load a image.
A texture identifed by path can be used for multiple images that have different UVs.
A path is a relative one from image root.
A name is represented as a keyword."
  (check-type path string)
  (check-type name keyword)
  (let* ((tex-info (ensure-gethash path *texture-table*
                                   (init-texture-info path alpha-path)))
         (img-info (make-image-info
                      :texture-id (texture-info-id tex-info)
                      :uv uv)))
    (setf (gethash name *image-table*) img-info)
    (process-load-image img-info)))

(defun get-image-size (name)
  "Return image width and height as multiple values."
  (check-type name keyword)
  (multiple-value-bind (tex-info img-info)
      (get-texture-and-image name)
    (let ((uv (image-info-uv img-info))
          (tex-width (texture-info-width tex-info))
          (tex-height (texture-info-height tex-info)))
      (values (* tex-width (image-uv-width uv))
              (* tex-height (image-uv-height uv))))))

(defun get-image-id (name)
  (image-info-id (gethash name *image-table*)))

(defun set-image-path (resource-root-path relative-path)
  (setf *image-root-path*
        (merge-pathnames relative-path resource-root-path))
  (setf *image-relative-path* relative-path))

;; TODO: Functions to remove textures and images

;; --- internal --- ;;

(defun get-texture-and-image (image-name)
  (let* ((img-info (gethash image-name *image-table*))
         (tex-info (find-texture-info-by-id
                    (image-info-texture-id img-info))))
    (assert img-info)
    (values tex-info img-info)))

(defun find-texture-info-by-id (id)
  (maphash-values
   (lambda (tex-info)
     (when (= (texture-info-id tex-info) id)
       (return-from find-texture-info-by-id tex-info)))
   *texture-table*)
  (error "The id ~D is not inclueded in the texture info table." id))

(defun init-texture-info (relative-path relative-alpha-path)
  (assert *image-root-path*)
  (multiple-value-bind (width height)
      (read-image-size (merge-pathnames relative-path *image-root-path*))
    (let ((result (make-texture-info :path relative-path
                                     :alpha-path relative-alpha-path
                                     :width width :height height)))
      (process-load-texture result)
      result)))

;; Note: Only for PNG
(defun read-image-size (path)
  (let ((img (read-png-file path)))
    (with-image-bounds (width height) img
      (values width height))))

;; - sender - ;;

(defun process-load-texture (tex-info)
  (let ((alpha-path (texture-info-alpha-path tex-info)))
    (send-load-texture (get-frame-count) (incf-index-in-frame)
                       :path (namestring
                              (merge-pathnames (texture-info-path tex-info)
                                               *image-relative-path*))
                       ;; Note: In jonathan:to-json, nil is converted to "[]".
                       ;; But it is not interpreted as false in JavaScript.
                       ;; So use 0 instead of it.
                       ;; (But it is a dirty solution...)
                       :alpha-path (if alpha-path
                                       (namestring
                                        (merge-pathnames alpha-path
                                                         *image-relative-path*))
                                       0)
                       :texture-id (texture-info-id tex-info))))

(defun process-load-image (img-info)
  (let ((uv (image-info-uv img-info))
        (tex-info (find-texture-info-by-id (image-info-texture-id img-info))))
    (send-load-image (get-frame-count) (incf-index-in-frame)
                     :texture-id (texture-info-id tex-info)
                     :image-id (image-info-id img-info)
                     :uv-x (image-uv-x uv)
                     :uv-y (image-uv-y uv)
                     :uv-width (image-uv-width uv)
                     :uv-height (image-uv-height uv))))
