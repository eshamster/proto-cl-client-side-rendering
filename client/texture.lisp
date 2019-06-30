(defpackage proto-cl-client-side-rendering/client/texture
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :update-texture
           :texture-message-p
           :interpret-texture-message
           :image-loaded-p
           :make-image-mesh)
  (:import-from :proto-cl-client-side-rendering/protocol
                :code-to-name)
  (:import-from :alexandria
                :make-keyword))
(in-package :proto-cl-client-side-rendering/client/texture)

(enable-ps-experiment-syntax)

;; --- macro --- ;;

(defmacro.ps+ with-command-data ((&rest target-list) command &body body)
  `(let (,@(mapcar (lambda (target)
                     `(,target (get-data ,command ,(make-keyword target))))
                   target-list))
     ,@body))


;; --- data --- ;;

(defstruct.ps+ texture-info
  id
  bitmap-image)

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
     (with-command-data (path texture-id) command
       (load-texture :path path :id texture-id)))
    (:load-image
     (with-command-data (image-id texture-id uv-x uv-y uv-width uv-height)
         command
       (register-image :id image-id
                       :texture-id texture-id
                       :uv-x uv-x
                       :uv-y uv-y
                       :uv-width uv-width
                       :uv-height uv-height)))))

(defun.ps+ texture-message-p (kind-code)
  (case (code-to-name kind-code)
    ((:load-texture :load-image) t)
    (t nil)))

;; - for drawer - ;;

(defun.ps make-image-mesh (&key image-id width height color)
  (unless (image-loaded-p image-id)
    (return-from make-image-mesh
      (new (#j.THREE.Mesh#
            (make-image-geometry :width width
                                 :height height)
            (new (#j.THREE.MeshBasicMaterial#
                  (create :color #x888888)))))))
  (let ((img-info (find-image-info-by-image-id image-id))
        (tex-info (find-tex-info-by-image-id image-id)))
    (with-slots (uv-x uv-y uv-width uv-height) img-info
      (new (#j.THREE.Mesh#
            (make-image-geometry :width width
                                 :height height
                                 :uv-x uv-x
                                 :uv-y uv-y
                                 :uv-width uv-width
                                 :uv-height uv-height)
            (make-image-material :tex-info tex-info
                                 :color color))))))

;; --- internal --- ;;

(defun.ps load-texture (&key path id)
  (let ((loader (new (#j.THREE.TextureLoader#))))
    (loader.load path
                 (lambda (bitmap-image)
                   (console.log (+ path " has been loaded"))
                   (push (make-texture-info
                          :id id
                          :bitmap-image bitmap-image)
                         *texture-info-buffer*))
                 (lambda (err)
                   (console.log err)))))

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
    (setf geometry.vertices
          (list (new (#j.THREE.Vector3# 0 0 0))
                (new (#j.THREE.Vector3# width 0 0))
                (new (#j.THREE.Vector3# width height 0))
                (new (#j.THREE.Vector3# 0 height 0))))
    (setf geometry.faces
          (list (new (#j.THREE.Face3# 0 1 2))
                (new (#j.THREE.Face3# 2 3 0))))
    (let ((uv-x+ (+ uv-x uv-width))
          (uv-y+ (+ uv-y uv-height)))
      (setf (aref geometry.face-vertex-uvs 0)
            (list (list (new (#j.THREE.Vector2# uv-x  uv-y ))
                        (new (#j.THREE.Vector2# uv-x+ uv-y ))
                        (new (#j.THREE.Vector2# uv-x+ uv-y+)))
                  (list (new (#j.THREE.Vector2# uv-x+ uv-y+))
                        (new (#j.THREE.Vector2# uv-x  uv-y+))
                        (new (#j.THREE.Vector2# uv-x  uv-y ))))))
    (geometry.compute-face-normals)
    (geometry.compute-vertex-normals)
    (setf geometry.uvs-need-update t)
    geometry))

(defun.ps make-image-material (&key tex-info color)
  (new (#j.THREE.MeshBasicMaterial#
        (create map (texture-info-bitmap-image tex-info)
                ;; TODO: alpha-map alpha-bitmap
                ;; TODO: transparent (if alpha-bitmap true false)
                color color))))

(defun.ps+ get-data (command target)
  (gethash target (gethash :data command)))

(defun.ps+ find-image-info-by-image-id (image-id)
  (gethash image-id *image-info-table*))

(defun.ps+ find-tex-info-by-image-id (image-id)
  (let ((img-info (find-image-info-by-image-id image-id)))
    (when img-info
      (gethash (image-info-texture-id img-info)
               *texture-info-table*))))
