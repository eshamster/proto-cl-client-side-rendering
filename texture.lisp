(defpackage proto-cl-client-side-rendering/texture
  (:use :cl)
  (:export :update-texture
           :make-image-uv
           :load-texture
           :load-image
           :get-image-size
           :get-image-id
           :get-texture-id
           :set-image-path
           :get-image-root-path
           :get-image-relative-path)
  (:import-from :proto-cl-client-side-rendering/client-list-manager
                :with-sending-to-new-clients)
  (:import-from :proto-cl-client-side-rendering/frame-counter
                :get-frame-count
                :incf-index-in-frame)
  (:import-from :proto-cl-client-side-rendering/protocol
                :send-load-texture
                :send-load-image)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :*target-client-id-list*)
  (:import-from :alexandria
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

(defvar *texture-table* (make-hash-table)
  "Key: A name represented as a keyword; Value: texture-info")
(defvar *image-table* (make-hash-table)
  "Key: A name represented as a keyword; value: image-info")

(defstruct texture-info
  id
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
  (with-sending-to-new-clients ()
    (maphash-values (lambda (tex-info)
                      (process-load-texture tex-info))
                    *texture-table*)
    (maphash-values (lambda (img-info)
                      (process-load-image img-info))
                    *image-table*)))

(defun load-texture (&key name path alpha-path)
  "Load a texture.
A name is represented as a keyword.
A path and alpha-path are relative ones from image root."
  (check-type name keyword)
  (setf (gethash name *texture-table*)
        (init-texture-info (make-texture-id name) path alpha-path)))

(defun load-image (&key texture-name image-name (uv (make-image-uv)))
  "Load a image.
A texture-name and image-name are represented as keywords.
The texture-name should has been loaded by \"load-texture\".
A texture identifed by texture-name can be used for multiple images that have different UVs."
  (check-type image-name keyword)
  (let* ((tex-info (get-texture-info-by-name texture-name))
         (img-info (make-image-info
                      :texture-id (texture-info-id tex-info)
                      :uv uv)))
    (setf (gethash image-name *image-table*) img-info)
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

(defun get-texture-id (name)
  (texture-info-id (gethash name *texture-table*)))

(defun set-image-path (resource-root-path relative-path)
  (setf *image-root-path*
        (merge-pathnames relative-path resource-root-path))
  (setf *image-relative-path* relative-path))

(defun get-image-root-path ()
  *image-root-path*)

(defun get-image-relative-path ()
  *image-relative-path*)

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

(defun init-texture-info (id relative-path relative-alpha-path)
  (assert *image-root-path*)
  (multiple-value-bind (width height)
      (read-image-size (merge-pathnames relative-path *image-root-path*))
    (let ((result (make-texture-info :id id
                                     :path relative-path
                                     :alpha-path relative-alpha-path
                                     :width width :height height)))
      (process-load-texture result)
      result)))

(defun get-texture-info-by-name (texture-name)
  (let ((result (gethash texture-name *texture-table*)))
    (unless result
      (error "The texture \"~A\" has not been loaded." texture-name))
    result))

;; Note: Only for PNG
(defun read-image-size (path)
  (let ((img (read-png-file path)))
    (with-image-bounds (width height) img
      (values width height))))

(defun make-texture-id (name)
  (let ((info (gethash name *texture-table*)))
    (if info
        (texture-info-id info)
        (incf *texture-id*))))

;; - sender - ;;

(defun process-load-texture (tex-info)
  (let ((alpha-path (texture-info-alpha-path tex-info)))
    (send-load-texture (get-frame-count) (incf-index-in-frame)
                       :path (namestring
                              (merge-pathnames (texture-info-path tex-info)
                                               (get-image-relative-path)))
                       ;; Note: In jonathan:to-json, nil is converted to "[]".
                       ;; But it is not interpreted as false in JavaScript.
                       ;; So use 0 instead of it.
                       ;; (But it is a dirty solution...)
                       :alpha-path (if alpha-path
                                       (namestring
                                        (merge-pathnames alpha-path
                                                         (get-image-relative-path)))
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
