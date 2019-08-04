(defpackage proto-cl-client-side-rendering/client/utils
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :with-command-data
           :make-rect-vertices
           :make-rect-faces
           :make-rect-face-vertex-uvs
           :make-dummy-rect-mesh)
  (:import-from :alexandria
                :make-keyword))
(in-package :proto-cl-client-side-rendering/client/utils)

;; --- interface --- ;;

;; - message - ;;

(defmacro.ps+ with-command-data ((&rest target-list) command &body body)
  `(let (,@(mapcar (lambda (target)
                     `(,target (get-data ,command ,(make-keyword target))))
                   target-list))
     ,@body))

;; - graphics - ;;

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

(defun.ps make-dummy-rect-mesh (&key width height (color #x888888))
  (new (#j.THREE.Mesh#
        (make-dummy-rect-geometry width height)
        (new (#j.THREE.MeshBasicMaterial#
              (create :color color))))))

;; --- internal --- ;;

;; - message - ;;

(defun.ps+ get-data (command target)
  (gethash target (gethash :data command)))

;; - graphics - ;;

(defun.ps make-dummy-rect-geometry (width height)
  (let ((geometry (new (#j.THREE.Geometry#))))
    (setf geometry.vertices (make-rect-vertices width height 0 0))
    (setf geometry.faces (make-rect-faces 0))
    (geometry.compute-face-normals)
    (geometry.compute-vertex-normals)
    geometry))
