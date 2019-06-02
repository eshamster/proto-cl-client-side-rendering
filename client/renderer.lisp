(defpackage proto-cl-client-side-rendering/client/renderer
  (:use :cl)
  (:export :init-screen-size
           :set-screen-size
           :get-screen-size
           :get-screen-scale
           :get-rendered-dom)
  (:import-from :proto-cl-client-side-rendering/client/camera
                :init-camera)
  (:import-from :ps-experiment
                :defvar.ps
                :defvar.ps+
                :defun.ps
                :defun.ps+
                :enable-ps-experiment-syntax))
(in-package :proto-cl-client-side-rendering/client/renderer)

(enable-ps-experiment-syntax)

;; --- data --- ;;

;; TODO: Fix the following issue
;; This is a temporal solution to avoid unintentional scroll bar
;; when the height size eqauls to 100% of the screen height.
;; In such case, 7px area is appeared both in top and bottom.
;; But the cause is not revealed.
(defvar.ps+ *window-height-adjust* 14)
(defvar.ps+ *resize-to-screen-p* t)
(defvar.ps+ *rendered-dom* nil)
(defvar.ps+ *renderer* nil)

;; Note: The values of width (800) and height (600) are temporal.
;; They are immediately overwritten by ":set-screen-size" operation.
(defvar.ps+ *screen-width* 800)
(defvar.ps+ *screen-height* 600)
(defvar.ps+ *screen-scale* 1)

;; --- interface --- ;;

(defun.ps+ get-screen-size ()
  (values *screen-width* *screen-height*))

(defun.ps+ get-screen-scale ()
  *screen-scale*)

(defun.ps+ get-rendered-dom ()
  *rendered-dom*)

(defun.ps set-screen-size (screen-width screen-height)
  (unless *renderer*
    (error "The renderer is not initialized. Should call initalize-screen-size before set-screen-size."))
  (labels ((calc-scale ()
             (min (/ window.inner-width screen-width)
                  (/ (- window.inner-height *window-height-adjust*) screen-height)))
           (set-position-by-size (width height)
             (setf *rendered-dom*.style.position "absolute"
                   *rendered-dom*.style.left (+ (/ (- window.inner-width width) 2) "px")
                   *rendered-dom*.style.top (+ (/ (- window.inner-height height) 2) "px")))
           (set-size (width height)
             (*renderer*.set-size width height)
             (set-position-by-size width height)))
    (let ((scale (if *resize-to-screen-p* (calc-scale) 1)))
      (set-size (* screen-width scale)
                (* screen-height scale))
      (setf *screen-width* screen-width
            *screen-height* screen-height
            *screen-scale* scale))))

(defun.ps init-screen-size (rendered-dom renderer resize-to-screen-p)
  (setf *rendered-dom* rendered-dom
        *renderer* renderer
        *resize-to-screen-p* resize-to-screen-p)
  (set-screen-size *screen-width* *screen-height*)
  (let ((resize-timer nil))
    (window.add-event-listener
     "resize" (lambda (e)
                (declare (ignore e))
                (when resize-timer
                  (clear-timeout resize-timer))
                (setf resize-timer
                      (set-timeout (lambda ()
                                     (set-screen-size *screen-width* *screen-height*))
                                   100))))))
