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
  (up-p-table (make-hash-table))
  (count-table (make-hash-table)))

;; --- interface --- ;;

(defun update-key-input-info (info)
  (check-type info key-input-info)
  (let ((count-table (key-input-info-count-table info))
        (down-p-table (key-input-info-down-p-table info))
        (up-p-table (key-input-info-up-p-table info)))
    (maphash (lambda (key down-p)
               (let ((up-p (gethash key up-p-table)))
                 (multiple-value-bind (next-count next-down-p next-up-p)
                     (calc-next-input-state
                      (gethash key count-table 0) down-p up-p)
                   (setf (gethash key count-table) next-count
                         (gethash key down-p-table) next-down-p
                         (gethash key up-p-table) next-up-p))))
             down-p-table)))

(defun set-raw-key-state (info key down-p)
  (if down-p
      (setf (gethash key (key-input-info-down-p-table info))
            t)
      (setf (gethash key (key-input-info-up-p-table info))
            t)))

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

(defun calc-next-input-state (current key-down-p key-up-p)
  "Return values (next-count next-dow-p next-up-p)"
  (cond ((and key-down-p key-up-p) (values  1 nil t))
        (key-down-p                (values  1 nil nil))
        (key-up-p                  (values -1 nil nil))
        (t (values (if (> current 0)
                       (1+ current)
                       (1- current))
                   nil nil))))

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
