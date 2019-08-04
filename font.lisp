(defpackage proto-cl-client-side-rendering/font
  (:use :cl)
  (:export :load-font
           :update-font
           :get-font-id)
  (:import-from :proto-cl-client-side-rendering/client-list-manager
                :with-sending-to-new-clients)
  (:import-from :proto-cl-client-side-rendering/frame-counter
                :get-frame-count
                :incf-index-in-frame)
  (:import-from :proto-cl-client-side-rendering/protocol
                :send-load-font)
  (:import-from :proto-cl-client-side-rendering/texture
                :get-image-relative-path
                :get-texture-id)
  (:import-from :alexandria
                :maphash-values))
(in-package :proto-cl-client-side-rendering/font)

;; --- data --- ;;

(defvar *font-id* 0)

(defvar *font-table* (make-hash-table)
  "Key: A name represented as a keyword; Value: font-info")

(defstruct font-info
  id
  texture-id
  json-path ; path from image-root-path
  )

;; --- interface --- ;;

(defun update-font ()
  (with-sending-to-new-clients ()
    (maphash-values (lambda (font-info)
                      (process-load-font font-info))
                    *font-table*)))

(defun load-font (&key name texture-name json-path)
  "Load a texture.
A name and texture-name are represented as keywords.
A json-path that is a relative one from image root has information of positions of each character ."
  (check-type name keyword)
  (setf (gethash name *font-table*)
        (init-font-info :id (make-font-id name)
                        :texture-id (get-texture-id texture-name)
                        :json-path json-path)))

;; TODO: Functions to remove fonts

;; TODO: (defun calc-text-width (&key font-name text))

(defun get-font-id (name)
  (font-info-id (gethash name *font-table*)))

;; --- internal --- ;;

(defun init-font-info (&key id texture-id json-path)
  (let ((result (make-font-info :id id
                                :texture-id texture-id
                                :json-path json-path)))
    (process-load-font result)
    result))

(defun make-font-id (name)
  (let ((info (gethash name *font-table*)))
    (if info
        (font-info-id info)
        (incf *font-id*))))

;; - sender - ;;

(defun process-load-font (font-info)
  (send-load-font (get-frame-count) (incf-index-in-frame)
                  :font-id (font-info-id font-info)
                  :texture-id (font-info-texture-id font-info)
                  :font-json-path
                  (namestring
                   (merge-pathnames (font-info-json-path font-info)
                                    (get-image-relative-path)))))
