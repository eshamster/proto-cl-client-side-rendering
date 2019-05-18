(defpackage proto-cl-client-side-rendering/protocol
  (:use :cl)
  (:export :code-to-name
           :name-to-code
           :send-frame-start
           :send-frame-end
           :send-delete-draw-object
           :send-draw-rect
           :send-draw-circle
           :send-log-console
           :draw-code-p
           :bool-to-number
           :number-to-bool)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :send-from-server)
  (:import-from :jonathan
                :to-json)
  (:import-from :ps-experiment
                :defvar.ps+
                :defun.ps+
                :def-top-level-form.ps+))
(in-package :proto-cl-client-side-rendering/protocol)

(defvar.ps+ *code-to-name-table* nil)
(defvar.ps+ *name-to-code-table* nil)

(defun.ps+ initialize-table ()
  (setf *code-to-name-table* (make-hash-table)
        *name-to-code-table* (make-hash-table))
  (dolist (pair '(;  server to client
                  (0 :frame-start)
                  (1 :frame-end)
                  (10 :delete-draw-object)
                  (11 :draw-rect)
                  (12 :draw-circle)
                  (21 :log-console)
                  ;; client to server
                  (-1 :key-down)
                  (-2 :key-up)
                  (-11 :mouse-down)
                  (-12 :mouse-up)
                  (-13 :mouse-move)
                  (-21 :touch-start)
                  (-22 :touch-end)
                  (-23 :touch-move)
                  (-24 :touch-cancel) ; not implemented
                  ))
    (let ((code (car pair))
          (name (cadr pair)))
      (setf (gethash code *code-to-name-table*) name
            (gethash name *name-to-code-table*) code))))

(def-top-level-form.ps+ :call-protocol-initializer
  (initialize-table))

(defun.ps+ code-to-name (code)
  (gethash code *code-to-name-table*))

(defun.ps+ name-to-code (name)
  (gethash name *name-to-code-table*))

;; --- utils --- ;;

(defun.ps+ draw-code-p (code)
  (let ((target-name (code-to-name code)))
    (some (lambda (name)
            (eq name target-name))
          '(:delete-draw-object :draw-rect :draw-circle))))

(defun.ps+ bool-to-number (bool)
  (if bool 1 0))

(defun.ps+ number-to-bool (number)
  (if (= number 1) t nil))

;; --- sender --- ;;

(defun down-case-keyword (data)
  (labels ((down (keyword)
             (intern (string-downcase (symbol-name keyword))
                     "KEYWORD"))
           (rec (lst)
             (symbol-macrolet ((head (car lst)))
               (if (listp head)
                   (when head
                     (rec head))
                   (when (keywordp head)
                     (setf head (down head)))))
             (when (cdr lst)
               (rec (cdr lst)))))
    (rec data)
    data))

(defun send-message (kind-name frame index-in-frame data)
  (send-from-server
   (to-json (down-case-keyword `(:kind ,(name-to-code kind-name)
                                 :frame ,frame
                                 :no ,index-in-frame
                                 :data ,data)))))

;; - start and end - ;;

(defun send-frame-start (frame index-in-frame)
  (send-message :frame-start frame index-in-frame '()))

(defun send-frame-end (frame index-in-frame)
  (send-message :frame-end frame index-in-frame '()))

;; - draw - ;;

(defun send-delete-draw-object (frame index-in-frame &key id)
  (send-message :delete-draw-object frame index-in-frame
                `(:id ,id)))

(defun send-draw-message (kind-name frame index-in-frame data
                          &key id x y depth color)
  (send-message kind-name frame index-in-frame
                `(:id ,id :x ,x :y ,y :depth ,depth :color ,color ,@data)))

(defun send-draw-rect (frame index-in-frame
                       &key id x y depth color fill-p width height rotate)
  (send-draw-message :draw-rect frame index-in-frame
                     `(:fill-p ,(bool-to-number fill-p)
                       :width ,width :height ,height :rotate ,rotate)
                     :id id
                     :x x :y y :depth depth :color color))

(defun send-draw-circle (frame index-in-frame &key id x y depth color fill-p r)
  (send-draw-message :draw-circle frame index-in-frame
                     `(:fill-p ,(bool-to-number fill-p) :r ,r)
                     :id id
                     :x x :y y :depth depth :color color))

;; - log - ;;

(defun send-log-console (frame index-in-frame &key message)
  (send-message :log-console frame index-in-frame
                `(:message ,message)))
