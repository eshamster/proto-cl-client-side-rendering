(defpackage proto-cl-client-side-rendering/protocol
  (:use :cl)
  (:export :code-to-name
           :name-to-code
           :send-frame-start
           :send-frame-end
           :send-delete-draw-object
           :send-draw-rect
           :send-draw-circle
           :send-draw-line
           :send-draw-arc
           :send-set-screen-size
           :send-set-camera
           :send-log-console
           :send-load-texture
           :send-load-image
           :send-draw-image
           :send-load-font
           :send-draw-text
           :draw-code-p
           :texture-code-p
           :font-code-p
           :bool-to-number
           :number-to-bool)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :send-from-server
                :*target-client-id-list*
                :same-target-client-list-p
                :copy-target-client-id-list)
  (:import-from :alexandria
                :appendf
                :make-keyword)
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
                  (13 :draw-arc)
                  (15 :draw-line)
                  ;; Note: One texture can include multiple images.
                  (20 :load-texture)
                  (21 :load-image)
                  (22 :draw-image)
                  (25 :load-font)
                  (26 :draw-text)
                  (51 :set-screen-size)
                  (55 :set-camera)
                  (101 :log-console)
                  ;; client to server
                  (-1 :key-down)
                  (-2 :key-up)
                  (-11 :mouse-down)
                  (-12 :mouse-up)
                  (-13 :mouse-move)
                  (-14 :mouse-wheel)
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
          '(:delete-draw-object :draw-rect :draw-circle :draw-line :draw-arc
            :draw-image :draw-text))))

(defun.ps+ texture-code-p (code)
  (let ((target-name (code-to-name code)))
    (some (lambda (name)
            (eq name target-name))
          '(:load-texture :load-image))))

(defun.ps+ font-code-p (code)
  (let ((target-name (code-to-name code)))
    (some (lambda (name)
            (eq name target-name))
          '(:load-font))))

(defun.ps+ bool-to-number (bool)
  (if bool 1 0))

(defun.ps+ number-to-bool (number)
  (if (= number 1) t nil))

;; --- sender --- ;;

(defvar *pre-target-client-id-list* nil)
(defvar *message-buffer* nil)
(defparameter *max-message-buffer* 10) ; Not well-considered value

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

(defun send-messages-in-buffer ()
  (send-from-server (to-json *message-buffer*))
  (setf *message-buffer* nil))

(defun send-message (kind-name frame index-in-frame data)
  (unless (same-target-client-list-p *pre-target-client-id-list*
                                     *target-client-id-list*)
    (let ((*target-client-id-list* *pre-target-client-id-list*))
      (send-messages-in-buffer)))
  (setf *pre-target-client-id-list* (copy-target-client-id-list))
  (push (down-case-keyword `(:kind ,(name-to-code kind-name)
                             :frame ,frame
                             :no ,index-in-frame
                             :data ,data))
        *message-buffer*)
  (when (or (eq kind-name :frame-end)
            (>= (length *message-buffer*) *max-message-buffer*))
    (send-messages-in-buffer)))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defvar *sender-table* (make-hash-table)
    "Key: sender name, Value: arg-list")

  (defun map-atom-or-head (list)
    "Ex. (a b (c :bool) d) -> (a b c d)"
    (mapcar (lambda (elem)
              (if (atom elem) elem (car elem)))
            list))

  (defun map-send-data (data-list)
    "Ex. (a b (c :bool) d) -> (:a a :b b :c (bool-to-number c) :d d)"
    (mapcan (lambda (elem)
              (let* ((wrapped (if (atom elem) (list elem) elem))
                     (name (car  wrapped))
                     (type (cadr wrapped)))
                (list (make-keyword name)
                      (if type
                          (ecase type
                            (:bool `(bool-to-number ,name)))
                          name))))
            data-list)))

(defmacro def-sender (name protocol (&key include) &rest data-list)
  (let ((all-data-list data-list))
    (when include
      (multiple-value-bind (included-list found)
          (gethash include *sender-table*)
        (unless found
          (error "The \"~A\" included is not defined." include))
        (appendf all-data-list included-list)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *sender-table*) ',all-data-list)
       ,(when protocol
          `(defun ,name (frame index-in-frame
                         &key ,@(map-atom-or-head all-data-list))
             (send-message ,protocol frame index-in-frame
                           (list ,@(map-send-data all-data-list)))))
       ',name)))

;; - start and end - ;;

(def-sender send-frame-start :frame-start ())
(def-sender send-frame-end :frame-end ())

;; - draw - ;;

(def-sender send-draw-message nil ()
            id x y depth color)

(def-sender send-delete-draw-object :delete-draw-object ()
            id)

(def-sender send-draw-rect :draw-rect (:include send-draw-message)
            (fill-p :bool) width height rotate)

(def-sender send-draw-circle :draw-circle (:include send-draw-message)
            (fill-p :bool) r)

(def-sender send-draw-line :draw-line (:include send-draw-message)
            x1 y1 x2 y2)

(def-sender send-draw-arc :draw-arc (:include send-draw-message)
            r start-angle sweep-angle)

;; image ;;

(def-sender send-load-texture :load-texture ()
            path alpha-path texture-id)

(def-sender send-load-image :load-image ()
            texture-id image-id uv-x uv-y uv-width uv-height)

(def-sender send-draw-image :draw-image (:include send-draw-message)
            image-id width height rotate)

;; text ;;

(def-sender send-load-font :load-font ()
            texture-id font-id font-info-json-path)

(def-sender send-draw-text :draw-text (:include send-draw-message)
            text font-id width height)

;; - screen size - ;;

(def-sender send-set-screen-size :set-screen-size ()
            width height)

;; - camera - ;;

(def-sender send-set-camera :set-camera ()
            center-x center-y scale)

;; - log - ;;

(def-sender send-log-console :log-console ()
            message)
