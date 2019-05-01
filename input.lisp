(defpackage proto-cl-client-side-rendering/input
  (:use :cl)
  (:export :update-input

           :key-down-now-p
           :key-down-p
           :key-up-now-p
           :key-up-p)
  (:import-from :proto-cl-client-side-rendering/protocol
                :name-to-code)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :register-message-processor)
  (:import-from :alexandria
                :make-keyword))
(in-package :proto-cl-client-side-rendering/input)

;; TODO: Discard information of disconnected client

(progn
  (defun process-input-message (client-id message-table)
    (set-raw-key-state client-id
                       (make-keyword
                        (string-upcase (gethash :key message-table)))
                       (= (gethash :kind message-table)
                          (name-to-code :key-down))))

  (register-message-processor 'input-processor #'process-input-message))

(defun update-input ()
  (maphash (lambda (id info)
             (declare (ignore id))
             (let ((count-table (client-input-info-key-count-table info))
                   (key-down-p-table (client-input-info-key-down-p-table info)))
               (maphash (lambda (key down-p)
                          (setf (gethash key count-table)
                                (calc-next-input-count
                                 (gethash key count-table 0)
                                 down-p)))
                        key-down-p-table)))
           *client-input-info-table*))

(defun key-down-now-p (client-id key)
  (down-now-p (get-key-count client-id key)))
(defun key-down-p (client-id key)
  (down-p (get-key-count client-id key)))
(defun key-up-now-p (client-id key)
  (up-now-p (get-key-count client-id key)))
(defun key-up-p (client-id key)
  (up-p (get-key-count client-id key)))

;; --- internal --- ;;

(defstruct client-input-info
  (key-down-p-table (make-hash-table))
  (key-count-table (make-hash-table)))

(defparameter *client-input-info-table* (make-hash-table))

(defun get-client-info (client-id)
  (let ((info (gethash client-id *client-input-info-table*)))
    (if info
        info
        (setf (gethash client-id *client-input-info-table*)
              (make-client-input-info)))))

(defun set-raw-key-state (client-id key down-p)
  (setf (gethash key
                 (client-input-info-key-down-p-table
                  (get-client-info client-id)))
        down-p))

(defun get-key-count (client-id key)
  (gethash key
           (client-input-info-key-count-table
            (get-client-info client-id))
           -2)) ; -2 is ":up" state

;; - utils - ;;

(defun calc-next-input-count (current key-down-p)
  (if key-down-p
      (if (>= current 0) (1+ current) 1)
      (if (<= current 0) (1- current) -1)))

(defun get-input-state (row-count)
  (cond ((= row-count 1) :down-now)
        ((> row-count 1) :down)
        ((= row-count -1) :up-now)
        (t :up)))

(defun down-now-p (row-count)
  (let ((state (get-input-state row-count)))
    (or (eq state :down-now))))
(defun down-p (row-count)
  (let ((state (get-input-state row-count)))
    (or (eq state :down) (eq state :down-now))))
(defun up-now-p (row-count)
  (let ((state (get-input-state row-count)))
    (or (eq state :up-now))))
(defun up-p (row-count)
  (let ((state (get-input-state row-count)))
    (or (eq state :up) (eq state :up-now))))

(defun get-down-count (row-count)
  (if (down-p row-count) row-count 0))
(defun get-up-count (row-count)
  (if (up-p row-count) (* -1 row-count) 0))
