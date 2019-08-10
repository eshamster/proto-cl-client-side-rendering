(defpackage proto-cl-client-side-rendering/font-utils
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :font-info-common
           :dostring
           :get-total-uv-width
           :get-total-uv-height
           :get-char-info
           :parse-raw-char-info
           :char-uv-info-x
           :char-uv-info-y
           :char-uv-info-width
           :char-uv-info-height
           :char-uv-info-origin-x
           :char-uv-info-origin-y
           :char-uv-info-advance))
(in-package :proto-cl-client-side-rendering/font-utils)

;; Common codes for server and client.

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

(defstruct.ps+ font-info-common
  id
  texture-id
  uv-top
  uv-bottom
  ;; key: char, value: char-uv-info
  (char-info-table (make-hash-table)))

;; --- interface --- ;;

(defun.ps+ parse-raw-char-info (&key id texture-id info-table)
  (let ((tex-width (gethash "width" info-table))
        (tex-height (gethash "height" info-table))
        (char-info-table (make-hash-table))
        (most-uv-top 0)
        (most-uv-bottom 0))
    (maphash (lambda (char info)
               (let ((uv-height (/ (gethash "height" info) tex-height))
                     (uv-origin-y (/ (gethash "originY" info) tex-height)))
                 (let ((uv-top uv-origin-y)
                       (uv-bottom (- uv-origin-y uv-height)))
                   (setf most-uv-top (max uv-top most-uv-top))
                   (setf most-uv-bottom (min uv-bottom most-uv-bottom)))
                 (setf (gethash char char-info-table)
                       (make-char-uv-info
                        :x (/ (gethash "x" info) tex-width)
                        :y (- 1.0 (/ (gethash "y" info) tex-height) uv-height)
                        :width (/ (gethash "width" info) tex-width)
                        :height uv-height
                        :origin-x (/ (gethash "originX" info) tex-width)
                        :origin-y uv-origin-y
                        :advance (/ (gethash "advance" info) tex-width)))))
             (gethash "characters" info-table))
    (make-font-info-common :id id
                    :texture-id texture-id
                    :uv-top most-uv-top
                    :uv-bottom most-uv-bottom
                    :char-info-table char-info-table)))

(defun.ps+ get-total-uv-width (text font-info-common)
  (let ((total-uv-width 0))
    (dostring (char text)
      (let ((char-info (get-char-info char font-info-common)))
        (incf total-uv-width (char-uv-info-advance char-info))))
    total-uv-width))

(defun.ps+ get-total-uv-height (text font-info-common)
  (declare (ignore text))
  (- (font-info-common-uv-top font-info-common)
     (font-info-common-uv-bottom font-info-common)))

(defun.ps+ get-char-info (char font-info-common)
  (gethash char (font-info-common-char-info-table font-info-common)))
