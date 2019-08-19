(defpackage proto-cl-client-side-rendering/utils/buffered-queue
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :init-buffered-queue
           :queue-to-buffer
           :dequeue-list-from-buffer))
(in-package :proto-cl-client-side-rendering/utils/buffered-queue)

;; --- data --- ;;

(defstruct.ps+ buffered-queue
  (queue (list))
  min-count
  max-count
  start-count
  (state :store) ; :store :normal
  )

;; --- interface --- ;;

(defun.ps+ init-buffered-queue (&key min-count start-count max-count)
  (assert (< min-count start-count max-count))
  (make-buffered-queue :min-count min-count
                       :start-count start-count
                       :max-count max-count))

(defun.ps+ queue-to-buffer (buffered-queue data-list)
  (with-slots (queue start-count state)
      buffered-queue
    (setf queue (append queue data-list))
    (when (and (eq state :store)
               (>= (length queue) start-count))
      (setf state :normal))))

(defun.ps+ dequeue-list-from-buffer (buffered-queue)
  (with-slots (queue min-count max-count start-count state)
      buffered-queue
    (when (eq state :store)
      (return-from dequeue-list-from-buffer (list)))
    (let ((before-count (length queue))
          (reversed-result (list)))
      (if (> before-count max-count)
          (dotimes (i (- before-count start-count))
            (push (pop queue) reversed-result))
          (push (pop queue) reversed-result))
      (when (< (length queue) min-count)
        (setf state :store))
      (reverse reversed-result))))
