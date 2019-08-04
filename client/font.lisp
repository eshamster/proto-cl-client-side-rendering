(defpackage proto-cl-client-side-rendering/client/font
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :update-font
           :interpret-font-message
           :make-text-mesh)
  (:import-from :proto-cl-client-side-rendering/protocol
                :code-to-name)
  (:import-from :proto-cl-client-side-rendering/client/texture
                :make-image-material
                :texture-loaded-p)
  (:import-from :proto-cl-client-side-rendering/client/utils
                :with-command-data)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :cl-ps-ecs
                :register-func-with-pred))
(in-package :proto-cl-client-side-rendering/client/font)

;; TODO: Convert not-defined character to a default character

(enable-ps-experiment-syntax)

;; --- macro --- ;;

(defmacro.ps dostring ((var text) &body body)
  (let ((i (gensym)))
    `(dotimes (,i (@ ,text length))
       (let ((,var (aref ,text ,i)))
         ,@body))))

(defmacro dostring ((var text) &body body)
  `(dolist (,var (coerce ,text 'list))
     ,@body))

;; --- data --- ;;

(defstruct.ps+ char-uv-info
    x y
    width height
    origin-x origin-y
    advance)

(defstruct.ps+ font-info
  id
  texture-id
  uv-top
  uv-bottom
  ;; key: char, value: char-uv-info
  (char-info-table (make-hash-table)))

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
     (with-command-data (font-id texture-id font-json-path)
         command
       (load-font :id font-id
                  :texture-id texture-id
                  :char-info-path font-json-path)))))

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
      (let ((result-mesh (new (#j.THREE.Mesh#
                               (make-dummy-geometry :width width
                                                    :height height)
                               (new (#j.THREE.MeshBasicMaterial#
                                     (create :color #x888888)))))))
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

(defun.ps load-font (&key id texture-id char-info-path)
  (let* ((loader (new (#j.THREE.FileLoader#))))
    (loader.load
     char-info-path
     (lambda (data)
       (let* ((raw-char-info (#j.JSON.parse# data))
              (tex-width raw-char-info.width)
              (tex-height raw-char-info.height)
              (char-info-table (make-hash-table))
              (most-uv-top 0)
              (most-uv-bottom 0))
         (maphash (lambda (char info)
                    (let ((uv-height (/ info.height tex-height))
                          (uv-origin-y (/ info.origin-y tex-height)))
                      (let ((uv-top uv-origin-y)
                            (uv-bottom (- uv-origin-y uv-height)))
                        (setf most-uv-top (max uv-top most-uv-top))
                        (setf most-uv-bottom (min uv-bottom most-uv-bottom)))
                      (setf (gethash char char-info-table)
                            (make-char-uv-info
                             :x (/ info.x tex-width)
                             :y (- 1.0 (/ info.y tex-height) uv-height)
                             :width (/ info.width tex-width)
                             :height uv-height
                             :origin-x (/ info.origin-x tex-width)
                             :origin-y uv-origin-y
                             :advance (/ info.advance tex-width)))))
                  raw-char-info.characters)
         (push (make-font-info :id id
                               :texture-id texture-id
                               :uv-top most-uv-top
                               :uv-bottom most-uv-bottom
                               :char-info-table char-info-table)
               *font-info-buffer*))))))

(defun.ps+ font-loaded-p (font-id)
  (let ((font-info (gethash font-id *font-info-table*)))
    (and font-info
         (texture-loaded-p (font-info-texture-id font-info)))))

;; - graphics - ;;

(defun.ps make-dummy-geometry (&key width height)
  (let ((geometry (new (#j.THREE.Geometry#))))
    (setf geometry.vertices (make-rect-vertices width height 0 0))
    (setf geometry.faces (make-rect-faces 0))
    (geometry.compute-face-normals)
    (geometry.compute-vertex-normals)
    geometry))

(defun.ps make-text-geometry (&key width height text font-info)
  (let* ((vertices (list))
         (faces (list))
         (face-vertex-uvs (list))
         (offset-x 0)
         (offset-face 0)
         (scale-x (/ width (get-total-uv-width text font-info)))
         (scale-y (/ height (get-total-uv-height text font-info)))
         (bottom (* (font-info-uv-bottom font-info) scale-y)))
    (dostring (char text)
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

(defun.ps make-rect-vertices (width height offset-x offset-y)
  (list (new (#j.THREE.Vector3# (+ offset-x 0) (+ offset-y 0) 0))
        (new (#j.THREE.Vector3# (+ offset-x width) (+ offset-y 0) 0))
        (new (#j.THREE.Vector3# (+ offset-x width) (+ offset-y height) 0))
        (new (#j.THREE.Vector3# (+ offset-x 0) (+ offset-y height) 0))))

(defun.ps make-rect-faces (offset)
  (list (new (#j.THREE.Face3# (+ offset 0)
                              (+ offset 1)
                              (+ offset 2)))
        (new (#j.THREE.Face3# (+ offset 2)
                              (+ offset 3)
                              (+ offset 0)))))

(defun.ps make-rect-face-vertex-uvs (uv-x uv-y uv-width uv-height)
  (let ((uv-x+ (+ uv-x uv-width))
        (uv-y+ (+ uv-y uv-height)))
    (list (list (new (#j.THREE.Vector2# uv-x  uv-y ))
                (new (#j.THREE.Vector2# uv-x+ uv-y ))
                (new (#j.THREE.Vector2# uv-x+ uv-y+)))
          (list (new (#j.THREE.Vector2# uv-x+ uv-y+))
                (new (#j.THREE.Vector2# uv-x  uv-y+))
                (new (#j.THREE.Vector2# uv-x  uv-y ))))))

(defun.ps+ get-total-uv-width (text font-info)
  (let ((total-uv-width 0))
    (dostring (char text)
      (let ((char-info (get-char-info char font-info)))
        (incf total-uv-width (char-uv-info-advance char-info))))
    total-uv-width))

(defun.ps+ get-total-uv-height (text font-info)
  (declare (ignore text))
  (- (font-info-uv-top font-info)
     (font-info-uv-bottom font-info)))

(defun.ps+ get-char-info (char font-info)
  (gethash char (font-info-char-info-table font-info)))

;; - char info - ;;

(defun.ps+ get-char-uv-width (char-info)
  (values (char-uv-info-width char-info)
          (char-uv-info-origin-x char-info)))

(defun.ps+ get-char-uv-height (char-info)
  (values (char-uv-info-height char-info)
          (char-uv-info-origin-y char-info)))

;; TODO: unload-font

