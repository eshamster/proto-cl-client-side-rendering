(defpackage proto-cl-client-side-rendering/font
  (:use :cl)
  (:export :load-font
           :update-font
           :get-font-id
           :calc-text-width
           :calc-text-height)
  (:import-from :proto-cl-client-side-rendering/client-list-manager
                :with-sending-to-new-clients)
  (:import-from :proto-cl-client-side-rendering/frame-counter
                :get-frame-count
                :incf-index-in-frame)
  (:import-from :proto-cl-client-side-rendering/font-utils
                :font-info-common
                :init-font-info-common
                :get-total-width
                :get-total-height)
  (:import-from :proto-cl-client-side-rendering/protocol
                :send-load-font)
  (:import-from :proto-cl-client-side-rendering/texture
                :get-image-relative-path
                :get-image-root-path
                :get-texture-id)
  (:import-from :alexandria
                :maphash-values))
(in-package :proto-cl-client-side-rendering/font)

;; --- data --- ;;

(defvar *font-id* 0)

(defvar *font-table* (make-hash-table)
  "Key: A name represented as a keyword; Value: font-info")

(defstruct (font-info (:include font-info-common))
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

(defun calc-text-width (&key font-name text height)
  (let* ((font-info (gethash font-name *font-table*))
         (raw-width (get-total-width text font-info))
         (raw-height (get-total-height text font-info)))
    (* height (/ raw-width raw-height))))

(defun calc-text-height (&key font-name text width)
  (let* ((font-info (gethash font-name *font-table*))
         (raw-width (get-total-width text font-info))
         (raw-height (get-total-height text font-info)))
    (* width (/ raw-height raw-width))))

(defun get-font-id (name)
  (font-info-id (gethash name *font-table*)))

;; --- internal --- ;;

(defun init-font-info (&key id texture-id json-path)
  (let ((result (init-font-info-common
                 :font-info (make-font-info :json-path json-path)
                 :id id
                 :texture-id texture-id
                 :info-table (parse-char-info-json-to-table json-path))))
    (process-load-font result)
    result))

(defun make-font-id (name)
  (let ((info (gethash name *font-table*)))
    (if info
        (font-info-id info)
        (incf *font-id*))))

(defun parse-char-info-json-to-table (json-relative-path)
  (let ((json-full-path (merge-pathnames json-relative-path (get-image-root-path))))
    (with-open-file (json json-full-path :direction :input)
      (let ((buf (make-string (file-length json))))
        (read-sequence buf json)
        (jonathan:parse buf :as :hash-table)))))

;; - sender - ;;

(defun process-load-font (font-info)
  (send-load-font (get-frame-count) (incf-index-in-frame)
                  :font-id (font-info-id font-info)
                  :texture-id (font-info-texture-id font-info)
                  :font-info-json-path
                  (namestring
                   (merge-pathnames (font-info-json-path font-info)
                                    (get-image-relative-path)))))
