(defpackage proto-cl-client-side-rendering/client/texture
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :update-texture
           :interpret-texture-message
           :make-image-mesh
           :make-image-material
           :texture-loaded-p)
  (:import-from :proto-cl-client-side-rendering/protocol
                :code-to-name)
  (:import-from :proto-cl-client-side-rendering/client/utils
                :with-command-data
                :make-rect-vertices
                :make-rect-faces
                :make-rect-face-vertex-uvs
                :make-dummy-rect-mesh)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :cl-ps-ecs
                :register-func-with-pred))
(in-package :proto-cl-client-side-rendering/client/texture)

(enable-ps-experiment-syntax)

;; --- data --- ;;

(defstruct.ps+ texture-info
  id
  bitmap-image
  alpha-bitmap-image)

(defstruct.ps+ image-info
  id
  texture-id
  uv-x uv-y uv-width uv-height)

(defvar.ps+ *texture-info-buffer* (list))
(defvar.ps+ *texture-info-table* (make-hash-table)
  "Key: texture id, Value: texture-info")
(defvar.ps+ *image-info-table* (make-hash-table)
  "Key: image id, Value: image-info")


;; --- interface --- ;;

;; - for general processor - ;;

(defun.ps+ update-texture ()
  (dolist (tex-info *texture-info-buffer*)
    (setf (gethash (texture-info-id tex-info)
                   *texture-info-table*)
          tex-info))
  ;; XXX: Assure all information in buffer is moved
  (setf *texture-info-buffer* (list)))

;; - for messenger - ;;

(defun.ps+ interpret-texture-message (kind-code command)
  (ecase (code-to-name kind-code)
    (:load-texture
     (with-command-data (path alpha-path texture-id) command
       (load-texture :path path :id texture-id
                     :alpha-path alpha-path)))
    (:load-image
     (with-command-data (image-id texture-id uv-x uv-y uv-width uv-height)
         command
       (register-image :id image-id
                       :texture-id texture-id
                       :uv-x uv-x
                       :uv-y uv-y
                       :uv-width uv-width
                       :uv-height uv-height)))))

;; - for drawer - ;;

(defun.ps make-image-mesh (&key image-id width height color)
  (flet ((make-geometry-and-material ()
           (let* ((img-info (find-image-info-by-image-id image-id))
                  (tex-id (image-info-texture-id img-info)))
             (values
              (with-slots (uv-x uv-y uv-width uv-height) img-info
                (make-image-geometry :width width
                                     :height height
                                     :uv-x uv-x
                                     :uv-y uv-y
                                     :uv-width uv-width
                                     :uv-height uv-height))
              (make-image-material :tex-id tex-id
                                   :color color)))))
    ;; If the image has not been loaded, returns a temoral mesh with
    ;; same width, height, and monochromatic. Then, rewrites by the image
    ;; after loading it.
    (unless (image-loaded-p image-id)
      (let ((result-mesh (make-dummy-rect-mesh :width width :height height)))
        (register-func-with-pred
         (lambda ()
           (multiple-value-bind (geometry material)
               (make-geometry-and-material)
             (setf result-mesh.geometry geometry
                   result-mesh.material material)))
         (lambda () (image-loaded-p image-id)))
        (return-from make-image-mesh
          result-mesh)))
    ;; The case where the image has been loaded.
    (multiple-value-bind (geometry material)
        (make-geometry-and-material)
      (new (#j.THREE.Mesh# geometry material)))))

(defun.ps make-image-material (&key tex-id color)
  (let* ((tex-info (gethash tex-id *texture-info-table*))
         (alpha-bitmap (texture-info-alpha-bitmap-image tex-info)))
    (new (#j.THREE.MeshBasicMaterial#
          (create map (texture-info-bitmap-image tex-info)
                  alpha-map alpha-bitmap
                  transparent (if alpha-bitmap true false)
                  color color)))))

(defun.ps+ texture-loaded-p (tex-id)
  (gethash tex-id *texture-info-table*))

;; --- internal --- ;;

(defun.ps load-texture (&key path alpha-path id)
  (let* ((loader (new (#j.THREE.TextureLoader#)))
         (image-promise
          (make-texture-load-promise loader path))
         (alpha-image-promise
          (make-texture-load-promise loader alpha-path)))
    (chain -promise
           (all (list image-promise alpha-image-promise))
           (then (lambda (images)
                   (push (make-texture-info
                          :id id
                          :bitmap-image (aref images 0)
                          :alpha-bitmap-image (aref images 1))
                         *texture-info-buffer*))))))

(defun.ps make-texture-load-promise (loader path)
  (new (-promise
        (lambda (resolve reject)
          (if path
              (loader.load path
                           (lambda (bitmap-image)
                             (console.log (+ path " has been loaded"))
                             (funcall resolve bitmap-image))
                           (lambda (err)
                             (console.log err)
                             (funcall reject err)))
              (funcall resolve nil))))))

(defun.ps+ image-loaded-p (image-id)
  (find-tex-info-by-image-id image-id))

(defun.ps+ register-image (&key id texture-id
                                uv-x uv-y uv-width uv-height)
  (setf (gethash id *image-info-table*)
        (make-image-info :id id
                         :texture-id texture-id
                         :uv-x uv-x
                         :uv-y uv-y
                         :uv-width uv-width
                         :uv-height uv-height)))

;; TODO: unload-texture

(defun.ps make-image-geometry (&key width height
                                    (uv-x 0) (uv-y 0) (uv-width 1) (uv-height 1))
  (let ((geometry (new (#j.THREE.Geometry#))))
    (setf geometry.vertices (make-rect-vertices width height 0 0)
          geometry.faces (make-rect-faces 0)
          (aref geometry.face-vertex-uvs 0) (make-rect-face-vertex-uvs
                                             uv-x uv-y uv-width uv-height))
    (geometry.compute-face-normals)
    (geometry.compute-vertex-normals)
    (setf geometry.uvs-need-update t)
    geometry))

(defun.ps+ find-image-info-by-image-id (image-id)
  (gethash image-id *image-info-table*))

(defun.ps+ find-tex-info-by-image-id (image-id)
  (let ((img-info (find-image-info-by-image-id image-id)))
    (when img-info
      (gethash (image-info-texture-id img-info)
               *texture-info-table*))))
