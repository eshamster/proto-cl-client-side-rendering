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
           :get-mouse-pos
           :get-wheel-delta-y

           :touch-summary-down-now-p
           :touch-summary-down-p
           :touch-summary-up-now-p
           :touch-summary-up-p
           :get-touch-summary-pos)
  (:import-from :proto-cl-client-side-rendering/protocol
                :name-to-code
                :code-to-name)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :register-message-processor
                :register-callback-on-disconnecting)
  (:import-from :alexandria
                :make-keyword
                :ensure-gethash
                :hash-table-values))
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
          (gethash :y data)))
        (:mouse-wheel
         (update-mouse-wheel-buffer
          client-id
          (gethash :delta-y data)))
        ((:touch-start :touch-end :touch-move)
         (update-touch-info-by-event client-id kind data))
        (t (print-nested-hash-table message-table)))))

  (register-message-processor 'input-processor #'process-input-message))

(progn
  (defun process-on-disconnecting (client-id)
    (delete-keyboard-info client-id)
    (delete-mouse-info client-id)
    (delete-touch-info client-id))

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
  (update-mouse-info)
  ;; touch
  (update-touch-info))

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
  (let ((pos (gethash client-id *mouse-info-table*)))
    (values (mouse-info-x pos) (mouse-info-y pos))))

(defun get-wheel-delta-y (client-id)
  (mouse-info-delta-y (gethash client-id *mouse-info-table*)))

;; - touch - ;;

;; TODO: Define functions to get information of each touch.

(defun touch-summary-down-now-p (client-id)
  "Do one or more touches exist from this frame?"
  (let ((state (get-touch-state-summary client-id)))
    (or (eq state :down-now))))
(defun touch-summary-down-p (client-id)
  "Do one or more touches exist?"
  (let ((state (get-touch-state-summary client-id)))
    (or (eq state :down-now) (eq state :down))))
(defun touch-summary-up-now-p (client-id)
  "Do no touches exist from this frame?"
  (let ((state (get-touch-state-summary client-id)))
    (or (eq state :up-now))))
(defun touch-summary-up-p (client-id)
  "Do no touches exist?"
  (let ((state (get-touch-state-summary client-id)))
    (or (eq state :up-now) (eq state :up))))

(defun get-touch-summary-pos (client-id)
  "Return average point of all touches as (values x y).
If no touches exist, return nil"
  (let ((info-list (gethash client-id *touch-info-table*)))
    (unless info-list
      (return-from get-touch-summary-pos nil))
    (let ((sum-x 0)
          (sum-y 0)
          (num (length info-list)))
      (dolist (info info-list)
        (incf sum-x (touch-info-x info))
        (incf sum-y (touch-info-y info)))
      (values (/ sum-x num) (/ sum-y num)))))

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

(defun delete-keyboard-info (client-id)
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

(defstruct mouse-info
  (x 0) (x-buffer 0)
  (y 0) (y-buffer 0)
  (delta-y 0) (delta-y-buffer 0))

(defvar *mouse-info-table* (make-hash-table))

(defun mouse-button-to-key-name (button)
  (case button
    (:left :mouse-left)
    (:right :mouse-right)
    (:center :mouse-center)))

(defun update-mouse-pos-buffer (client-id x y)
  (let ((info (ensure-gethash client-id *mouse-info-table*
                              (make-mouse-info))))
    (setf (mouse-info-x-buffer info) x
          (mouse-info-y-buffer info) y)))

(defun update-mouse-wheel-buffer (client-id delta-y)
  (let ((info (ensure-gethash client-id *mouse-info-table*
                              (make-mouse-info))))
    (incf (mouse-info-delta-y-buffer info) delta-y)))

(defun update-mouse-info ()
  (dolist (info (hash-table-values *mouse-info-table*))
    (with-slots (x y x-buffer y-buffer
                   delta-y delta-y-buffer)
        info
      (setf x x-buffer
            y y-buffer
            delta-y delta-y-buffer
            delta-y-buffer 0))))

(defun delete-mouse-info (client-id)
  (remhash client-id *mouse-info-table*))

;; - touch - ;;

(defvar *latest-touch-id* 0)

(defstruct touch-info
  (id (incf *latest-touch-id*))
  raw-id
  x y x-buffer y-buffer
  (down-p t)
  (count 0))

(defvar *touch-info-table* (make-hash-table)
  "Key: client id, Value list of touch-info")

(defun update-touch-info-by-event (client-id kind data-table-list)
  (let ((info-list (gethash client-id *touch-info-table* (list))))
    (dolist (data-table data-table-list)
      (let ((x (gethash :x data-table))
            (y (gethash :y data-table))
            (raw-id (gethash :id data-table)))
        (flet ((get-info ()
                 (find raw-id info-list
                       :key (lambda (info) (touch-info-raw-id info)))))
          (ecase kind
            (:touch-start
             ;; FIXME: Should prevent from getting new touch info until next frame.
             (push (make-touch-info :raw-id raw-id
                                    :x x :x-buffer x
                                    :y y :y-buffer y)
                   info-list))
            (:touch-end
             (let ((info (get-info)))
               (setf (touch-info-raw-id info) nil
                     (touch-info-x-buffer info) x
                     (touch-info-y-buffer info) y
                     (touch-info-down-p info) nil)))
            (:touch-move
             (let ((info (get-info)))
               (setf (touch-info-x-buffer info) x
                     (touch-info-y-buffer info) y)))))))
    (setf (gethash client-id *touch-info-table*) info-list)))

(defun update-touch-info ()
  (maphash (lambda (client-id info-list)
             (dolist (info info-list)
               (with-slots (x y x-buffer y-buffer
                              count down-p) info
                 (setf x x-buffer
                       y y-buffer
                       count (calc-next-input-count count down-p))))
             (setf (gethash client-id *touch-info-table*)
                   (remove-if (lambda (info)
                                (let ((count (touch-info-count info)))
                                  (and (not (up-now-p count))
                                       (up-p count))))
                              info-list)))
           *touch-info-table*))

(defun delete-touch-info (client-id)
  (remhash client-id *touch-info-table*))

(defun get-touch-state (client-id touch-id)
  (let* ((info-list (gethash client-id *touch-info-table* (list)))
         (info (find touch-id info-list :key #'touch-info-id)))
    (unless info
      (return-from get-touch-state :up))
    (get-input-state (touch-info-count info))))

(defun get-touch-state-summary (client-id)
  (let* ((info-list (gethash client-id *touch-info-table* (list)))
         (state-list (mapcar (lambda (info)
                               (get-input-state (touch-info-count info)))
                             info-list)))
    (case (length state-list)
      (0 :up)
      (1 (car state-list))
      (t (flet ((some-state (target-state)
                  (some (lambda (state) (eq state target-state))
                        state-list)))
           (cond ((some-state :down) :down)
                 ((some-state :down-now) :down-now)
                 ((some-state :up-now) :up-now)
                 (t :up)))))))

;; - utils - ;;

(defun calc-next-input-count (current key-down-p)
  (if key-down-p
      (if (>= current 0) (1+ current) 1)
      (if (<= current 0) (1- current) -1)))

(defun get-input-state (raw-count)
  (cond ((= raw-count 1) :down-now)
        ((> raw-count 1) :down)
        ((= raw-count -1) :up-now)
        (t :up)))

(defun down-now-p (raw-count)
  (let ((state (get-input-state raw-count)))
    (or (eq state :down-now))))
(defun down-p (raw-count)
  (let ((state (get-input-state raw-count)))
    (or (eq state :down) (eq state :down-now))))
(defun up-now-p (raw-count)
  (let ((state (get-input-state raw-count)))
    (or (eq state :up-now))))
(defun up-p (raw-count)
  (let ((state (get-input-state raw-count)))
    (or (eq state :up) (eq state :up-now))))

(defun get-down-count (raw-count)
  (if (down-p raw-count) raw-count 0))
(defun get-up-count (raw-count)
  (if (up-p raw-count) (* -1 raw-count) 0))

;; For debug when implementing a new event.
(defun print-nested-hash-table (table)
  (format t "~&-----------~%~A" (code-to-name (gethash :kind table)))
  (print (jonathan:parse (jonathan:to-json table))))
