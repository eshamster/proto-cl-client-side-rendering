(defpackage proto-cl-client-side-rendering/utils/input
  (:use :cl)
  (:export :make-key-input-info
           :update-key-input-info
           :input-down-now-p
           :input-down-p
           :input-up-now-p
           :input-up-p
           :set-raw-key-state
           :get-input-state))
(in-package :proto-cl-client-side-rendering/utils/input)

;; --- data --- ;;

(defstruct key-input-info
  (down-p-table (make-hash-table))
  (count-table (make-hash-table)))

;; --- interface --- ;;

(defun update-key-input-info (info)
  (check-type info key-input-info)
  (let ((count-table (key-input-info-count-table info))
        (key-down-p-table (key-input-info-down-p-table info)))
    (maphash (lambda (key down-p)
               (setf (gethash key count-table)
                     (calc-next-input-count
                      (gethash key count-table 0)
                      down-p)))
             key-down-p-table)))

(defun set-raw-key-state (info key down-p)
  (setf (gethash key (key-input-info-down-p-table info))
        down-p))

(defmacro def-input-judge (name judge-func)
  `(defun ,name (info key)
     (,judge-func (gethash key (key-input-info-count-table info) 0))))

(def-input-judge input-down-now-p down-now-p)
(def-input-judge input-down-p     down-p)
(def-input-judge input-up-now-p   up-now-p)
(def-input-judge input-up-p       up-p)

(defun get-input-state (info key)
  (get-input-state-by-count
   (gethash key (key-input-info-count-table info) 0)))

;; --- internal --- ;;

;; - utils - ;;

(defun calc-next-input-count (current key-down-p)
  (if key-down-p
      (if (>= current 0) (1+ current) 1)
      (if (<= current 0) (1- current) -1)))

(defun get-input-state-by-count (raw-count)
  (cond ((= raw-count 1) :down-now)
        ((> raw-count 1) :down)
        ((= raw-count -1) :up-now)
        (t :up)))

(defun down-now-p (raw-count)
  (let ((state (get-input-state-by-count raw-count)))
    (or (eq state :down-now))))
(defun down-p (raw-count)
  (let ((state (get-input-state-by-count raw-count)))
    (or (eq state :down) (eq state :down-now))))
(defun up-now-p (raw-count)
  (let ((state (get-input-state-by-count raw-count)))
    (or (eq state :up-now))))
(defun up-p (raw-count)
  (let ((state (get-input-state-by-count raw-count)))
    (or (eq state :up) (eq state :up-now))))

(defun get-down-count (raw-count)
  (if (down-p raw-count) raw-count 0))
(defun get-up-count (raw-count)
  (if (up-p raw-count) (* -1 raw-count) 0))
