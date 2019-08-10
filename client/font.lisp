(defpackage proto-cl-client-side-rendering/client/font
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :update-font
           :interpret-font-message
           :make-text-mesh)
  (:import-from :proto-cl-client-side-rendering/font-utils
                :font-info-common
                :dostring
                :get-total-uv-width
                :get-total-uv-height
                :get-char-info
                :init-font-info-common
                :char-uv-info-x
                :char-uv-info-y
                :char-uv-info-width
                :char-uv-info-height
                :char-uv-info-origin-x
                :char-uv-info-origin-y
                :char-uv-info-advance)
  (:import-from :proto-cl-client-side-rendering/protocol
                :code-to-name)
  (:import-from :proto-cl-client-side-rendering/client/texture
                :make-image-material
                :texture-loaded-p)
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
(in-package :proto-cl-client-side-rendering/client/font)

(enable-ps-experiment-syntax)

;; --- data --- ;;

(defstruct.ps+ (font-info (:include font-info-common)))

(defvar.ps+ *font-info-buffer* (list))

(defvar.ps+ *font-info-table* (make-hash-table)
  "Key: font id, Value: font-info")

;; --- interface --- ;;

;; - for general processor - ;;

(defun.ps+ update-font ()
  (dolist (font-info *font-info-buffer*)
    (setf (gethash (font-info-id font-info)
                   *font-info-table*)
          font-info))
  ;; XXX: Assure all information in buffer is moved
  (setf *font-info-buffer* (list)))

;; - for messenger - ;;

(defun.ps+ interpret-font-message (kind-code command)
  (ecase (code-to-name kind-code)
    (:load-font
     (with-command-data (font-id texture-id font-info-json-path)
         command
       (load-font :id font-id
                  :texture-id texture-id
                  :font-info-json-path font-info-json-path)))))

;; - for drawer - ;;

(defun.ps make-text-mesh (&key text font-id width height color)
  (flet ((make-geometry-and-material ()
           (let* ((font-info (gethash font-id *font-info-table*)))
             (values
              (make-text-geometry :width width
                                  :height height
                                  :text text
                                  :font-info font-info)
              (make-image-material :tex-id (font-info-texture-id font-info)
                                   :color color)))))
    ;; If the font has not been loaded, returns a temoral mesh with
    ;; same width, height, and monochromatic. Then, rewrites by the text
    ;; after loading it.
    (unless (font-loaded-p font-id)
      (let ((result-mesh (make-dummy-rect-mesh :width width :height height)))
        (register-func-with-pred
         (lambda ()
           (multiple-value-bind (geometry material)
               (make-geometry-and-material)
             (setf result-mesh.geometry geometry
                   result-mesh.material material)))
         (lambda () (font-loaded-p font-id)))
        (return-from make-text-mesh result-mesh)))
    ;; The case where the image has been loaded.
    (multiple-value-bind (geometry material)
        (make-geometry-and-material)
      (new (#j.THREE.Mesh# geometry material)))))

;; --- internal --- ;;

(defun.ps load-font (&key id texture-id font-info-json-path)
  (let* ((loader (new (#j.THREE.FileLoader#))))
    (loader.load
     font-info-json-path
     (lambda (data)
       (let ((info-table (#j.JSON.parse# data)))
         (push (init-font-info-common
                :font-info (make-font-info)
                :info-table info-table
                :id id
                :texture-id texture-id)
               *font-info-buffer*))))))

(defun.ps+ font-loaded-p (font-id)
  (let ((font-info (gethash font-id *font-info-table*)))
    (and font-info
         (texture-loaded-p (font-info-texture-id font-info)))))

;; - graphics - ;;

(defun.ps make-text-geometry (&key width height text font-info)
  (let* ((vertices (list))
         (faces (list))
         (face-vertex-uvs (list))
         (offset-x 0)
         (offset-face 0)
         (scale-x (/ width (get-total-uv-width text font-info)))
         (scale-y (/ height (get-total-uv-height text font-info)))
         (bottom (* (font-info-uv-bottom font-info) scale-y)))
    (dostring (char text font-info)
      (let ((char-info (get-char-info char font-info)))
        (multiple-value-bind (uv-width uv-origin-x)
            (get-char-uv-width char-info)
          (multiple-value-bind (uv-height uv-origin-y)
              (get-char-uv-height char-info)
            (let ((char-width    (* uv-width    scale-x))
                  (char-origin-x (* uv-origin-x scale-x))
                  (char-height   (* uv-height   scale-y))
                  (char-origin-y (* uv-origin-y scale-y))
                  (char-advance  (* (char-uv-info-advance char-info) scale-x)))
              (setf vertices
                    (append vertices
                            (make-rect-vertices
                             char-width char-height
                             (+ offset-x char-origin-x)
                             (- (- char-origin-y char-height) bottom))))
              (incf offset-x char-advance)
              (setf face-vertex-uvs
                    (append face-vertex-uvs
                            (make-rect-face-vertex-uvs
                             (char-uv-info-x char-info)
                             (char-uv-info-y char-info)
                             uv-width
                             uv-height)))))))
      (setf faces
            (append faces
                    (make-rect-faces offset-face)))
      (setf offset-face (length vertices)))
    (let ((geometry (new (#j.THREE.Geometry#))))
      (setf geometry.vertices vertices
            geometry.faces faces
            (aref geometry.face-vertex-uvs 0) face-vertex-uvs
            geometry.uvs-need-update t)
      (geometry.compute-face-normals)
      (geometry.compute-vertex-normals)
      geometry)))

;; - char info - ;;

(defun.ps+ get-char-uv-width (char-info)
  (values (char-uv-info-width char-info)
          (char-uv-info-origin-x char-info)))

(defun.ps+ get-char-uv-height (char-info)
  (values (char-uv-info-height char-info)
          (char-uv-info-origin-y char-info)))

;; TODO: unload-font

