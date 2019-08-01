(defpackage proto-cl-client-side-rendering/client/font
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :update-font
           :interpret-font-message)
  (:import-from :proto-cl-client-side-rendering/protocol
                :code-to-name)
  (:import-from :proto-cl-client-side-rendering/client/utils
                :with-command-data)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :cl-ps-ecs
                :register-func-with-pred))
(in-package :proto-cl-client-side-rendering/client/font)

(enable-ps-experiment-syntax)

;; --- data --- ;;

(defstruct.ps+ char-uv-info
    x y
    width height
    origin-x origin-y)

(defstruct.ps+ font-info
  id
  texture-id
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

;; TODO

;; --- internal --- ;;

(defun.ps load-font (&key id texture-id char-info-path)
  (let* ((loader (new (#j.THREE.FileLoader#))))
    (loader.load
     char-info-path
     (lambda (data)
       (let* ((raw-char-info (#j.JSON.parse# data))
              (tex-width raw-char-info.width)
              (tex-height raw-char-info.height)
              (char-info-table (make-hash-table)))
         (maphash (lambda (char info)
                    (setf (gethash char char-info-table)
                          (make-char-uv-info
                           :x (/ info.x tex-width)
                           :y (/ info.y tex-height)
                           :width (/ info.width tex-width)
                           :height (/ info.height tex-height)
                           :origin-x (/ info.origin-x tex-width)
                           :origin-y (/ info.origin-y tex-width))))
                  raw-char-info.characters)
         (push (make-font-info :id id
                               :texture-id texture-id
                               :char-info-table char-info-table)
               *font-info-buffer*))))))

(defun.ps+ image-loaded-p (image-id)
  (find-tex-info-by-image-id image-id))

;; TODO: unload-font

