(defpackage proto-cl-client-side-rendering/client/graphics
  (:use :cl)
  (:export :make-line
           :make-lines
           :make-solid-rect
           :make-wired-rect
           :make-solid-regular-polygon
           :make-wired-regular-polygon
           :make-arc
           :make-solid-circle
           :make-wired-circle
           :make-wired-polygon
           :make-solid-polygon
           :change-model-color
           :change-geometry-uvs

           :get-mesh-width
           :get-mesh-height
           :get-mesh-size)
  (:import-from :proto-cl-client-side-rendering/client/texture
                :image-loaded-p
                :make-image-mesh)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :parenscript
                :create
                :new)
  (:import-from :ps-experiment
                :enable-ps-experiment-syntax
                :defun.ps
                :defun.ps+
                :defmacro.ps+))
(in-package :proto-cl-client-side-rendering/client/graphics)

;; Note: This is ported from cl-web-2d-game

(enable-ps-experiment-syntax)

;; --- basic funcations and macros

(defun.ps push-vertices-to (geometry raw-vertex-lst)
  (dolist (vertex-as-lst raw-vertex-lst)
    (geometry.vertices.push
     (new (#j.THREE.Vector3# (nth 0 vertex-as-lst)
                             (nth 1 vertex-as-lst)
                             0)))))

(defun.ps push-faces-to (geometry raw-face-lst)
  (dolist (face-as-lst raw-face-lst)
    (geometry.faces.push
     (new (#j.THREE.Face3# (nth 0 face-as-lst)
                           (nth 1 face-as-lst)
                           (nth 2 face-as-lst))))))

(defun.ps+ to-rad (degree)
  (/ (* degree PI) 180))

(defun.ps make-line-model (geometry color)
  (let ((material (new (#j.THREE.LineBasicMaterial# (create :color color)))))
    (new (#j.THREE.Line# geometry material))))

(defmacro.ps+ def-wired-geometry (name args &body body)
  (let ((geometry (gensym)))
    `(defun.ps ,name (&key ,@args color)
       (let ((,geometry (new (#j.THREE.Geometry#))))
         (flet ((push-vertices (&rest rest)
                  (push-vertices-to ,geometry rest)))
           ,@body)
         (make-line-model ,geometry color)))))

(defun.ps make-solid-model (geometry color)
  (let ((material (new (#j.THREE.MeshBasicMaterial# (create :color color)))))
    (new (#j.THREE.Mesh# geometry material))))

(defmacro.ps+ def-solid-geometry (name args &body body)
  (let ((geometry (gensym)))
    `(defun.ps ,name (&key ,@args color)
       (let ((,geometry (new (#j.THREE.Geometry#))))
         (flet ((push-vertices (&rest rest)
                  (push-vertices-to ,geometry rest))
                (push-faces (&rest rest)
                  (push-faces-to ,geometry rest)))
           ,@body)
         (make-solid-model ,geometry color)))))

;; --- utils --- ;;

(defun.ps get-mesh-width (mesh)
  (- mesh.geometry.bounding-box.max.x
     mesh.geometry.bounding-box.min.x))

(defun.ps get-mesh-height (mesh)
  (- mesh.geometry.bounding-box.max.y
     mesh.geometry.bounding-box.min.y))

(defun.ps+ get-mesh-size (mesh)
  (list :width (get-mesh-width mesh)
        :height (get-mesh-height mesh)))

;; --- line --- ;;

(def-wired-geometry make-line (pos-a pos-b)
  (push-vertices (list (aref pos-a 0) (aref pos-a 1))
                 (list (aref pos-b 0) (aref pos-b 1))))

(def-wired-geometry make-lines (pnt-list)
  (dolist (pnt pnt-list)
    (push-vertices (list (aref pnt 0) (aref pnt 1)))))

;; --- rectangle --- ;;

(def-solid-geometry make-solid-rect (width height)
  (push-vertices (list 0 0) (list width 0)
                 (list width height) (list 0 height))
  (push-faces '(0 1 2) '(2 3 0)))

(def-wired-geometry make-wired-rect (width height)
  (push-vertices (list 0 0) (list width 0)
                 (list width height) (list 0 height)
                 (list 0 0)))

;; --- arc --- ;;

(def-wired-geometry make-wired-arc-with-vertex-count (r n start-angle sweep-angle)
  (dotimes (i n)
    (let ((angle (+ (/ (* sweep-angle i) (1- n)) start-angle)))
      (push-vertices (list (* r (cos angle))
                           (* r (sin angle)))))))

;; TODO: Adaptively decide the 'n' according to the 'r' and 'sweep-angle'
(defun.ps+ make-arc (&key start-angle sweep-angle r color)
  (make-wired-arc-with-vertex-count
   :color color :r r :start-angle start-angle :sweep-angle sweep-angle :n 60))

;; --- regular polygon --- ;;

(def-solid-geometry make-solid-regular-polygon (r n (start-angle 0))
  (dotimes (i n)
    (let ((angle (to-rad (+ (/ (* 360 i) n) start-angle))))
      (push-vertices (list (* r (cos angle))
                           (* r (sin angle))))))
  (push-vertices (list 0 0))
  (dotimes (i n)
    (push-faces (list n i (rem (1+ i) n)))))

(defun.ps+ make-wired-regular-polygon (&key color r n (start-angle 0))
  (make-wired-arc-with-vertex-count
   :color color :r r :start-angle start-angle :sweep-angle (* 2 PI) :n (1+ n)))

;; --- circle --- ;;
;; TODO: Adaptively decide the 'n' according to the 'r'

(defun.ps+ make-solid-circle (&key r color)
  (make-solid-regular-polygon :r r :n 60 :color color))

(defun.ps+ make-wired-circle (&key r color)
  (make-wired-regular-polygon :r r :n 60 :color color))

;; --- arbitrary polygon --- ;;

(def-wired-geometry make-wired-polygon (pnt-list)
  (dolist (pnt pnt-list)
    (push-vertices pnt))
  (push-vertices (nth 0 pnt-list)))

(def-solid-geometry make-solid-polygon (pnt-list)
  (dolist (pnt pnt-list)
    (push-vertices pnt))
  (let ((len (length pnt-list)))
    (dotimes (i (1- len))
      (push-faces (list 0
                        (+ i 1)
                        (rem (+ i 2) len))))))

;; --- auxiliary functions --- ;;

(defun.ps change-model-color (model-2d new-color-rgb)
  (with-slots (model) model-2d
    (setf model.material.color (new (#j.THREE.Color# new-color-rgb)))
    (setf model.material.needs-update t))
  model-2d)
