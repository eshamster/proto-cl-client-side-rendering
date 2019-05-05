(defpackage proto-cl-client-side-rendering/input
  (:use :cl)
  (:export :update-input

           :key-down-now-p
           :key-down-p
           :key-up-now-p
           :key-up-p

           :mouse-down-now-p
           :mouse-down-p
           :mouse-up-now-p
           :mouse-up-p
           :get-mouse-pos)
  (:import-from :proto-cl-client-side-rendering/protocol
                :name-to-code
                :code-to-name)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :register-message-processor
                :register-callback-on-disconnecting)
  (:import-from :alexandria
                :make-keyword))
(in-package :proto-cl-client-side-rendering/input)

(progn
  (defun process-input-message (client-id message-table)
    (let ((kind (code-to-name (gethash :kind message-table)))
          (data (gethash :data message-table)))
      (case kind
        ((:key-down :key-up)
         (set-raw-key-state client-id
                            (make-keyword
                             (string-upcase (gethash :key data)))
                            (eq kind :key-down)))
        ((:mouse-down :mouse-up)
         (let ((raw-button (gethash :button data)))
           ;; If raw-button is not string, the button is not implemented yet.
           (when (stringp raw-button)
             (let* ((button (make-keyword (string-upcase raw-button)))
                    (key-name (mouse-button-to-key-name button)))
               (when key-name
                 (set-raw-key-state client-id
                                    key-name
                                    (eq kind :mouse-down))))))
         (update-mouse-pos-buffer
          client-id
          (gethash :x data)
          (gethash :y data)))
        (:mouse-move
         (update-mouse-pos-buffer
          client-id
          (gethash :x data)
          (gethash :y data))))))

  (register-message-processor 'input-processor #'process-input-message))

(progn
  (defun process-on-disconnecting (client-id)
    (delete-client-info client-id))

  (register-callback-on-disconnecting 'input-callback #'process-on-disconnecting))

(defun update-input ()
  ;; keyboard
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
           *client-input-info-table*)
  ;; mouse
  (update-mouse-pos))

;; - keyboard - ;;

(defun key-down-now-p (client-id key)
  (down-now-p (get-key-count client-id key)))
(defun key-down-p (client-id key)
  (down-p (get-key-count client-id key)))
(defun key-up-now-p (client-id key)
  (up-now-p (get-key-count client-id key)))
(defun key-up-p (client-id key)
  (up-p (get-key-count client-id key)))

;; - mouse- ;;

(defun mouse-down-now-p (client-id button)
  (key-down-now-p client-id (mouse-button-to-key-name button)))
(defun mouse-down-p (client-id button)
  (key-down-p client-id (mouse-button-to-key-name button)))
(defun mouse-up-now-p (client-id button)
  (key-up-now-p client-id (mouse-button-to-key-name button)))
(defun mouse-up-p (client-id button)
  (key-up-p client-id (mouse-button-to-key-name button)))

(defun get-mouse-pos (client-id)
  "Returns (value x y)"
  (let ((pos (gethash client-id *mouse-pos-table*)))
    (values (mouse-pos-x pos) (mouse-pos-y pos))))

;; --- internal --- ;;

;; - keyboard (and mouse click) - ;;

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

(defun delete-client-info (client-id)
  (remhash client-id *client-input-info-table*))

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

;; - mouse - ;;

(defstruct mouse-pos x y x-buffer y-buffer)

(defvar *mouse-pos-table* (make-hash-table))

(defun mouse-button-to-key-name (button)
  (case button
    (:left :mouse-left)
    (:right :mouse-right)
    (:center :mouse-center)))

(defun update-mouse-pos-buffer (client-id x y)
  (let ((pos (gethash client-id *mouse-pos-table*)))
    (unless pos
      (setf pos (make-mouse-pos)
            (gethash client-id *mouse-pos-table*) pos))
    (setf (mouse-pos-x-buffer pos) x
          (mouse-pos-y-buffer pos) y)))

(defun update-mouse-pos ()
  (maphash (lambda (client-id pos)
             (declare (ignore client-id))
             (with-slots (x y x-buffer y-buffer) pos
               (setf x x-buffer
                     y y-buffer)))
           *mouse-pos-table*))

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
