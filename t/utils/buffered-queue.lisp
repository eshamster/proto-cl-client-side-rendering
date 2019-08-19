(defpackage proto-cl-client-side-rendering/t/utils/buffered-queue
  (:use :cl
        :rove
        :ps-experiment/t/test-utils
        :ps-experiment
        :proto-cl-client-side-rendering/utils/buffered-queue))
(in-package :proto-cl-client-side-rendering/t/utils/buffered-queue)

(defstruct.ps+ test-def
  desc
  min-count
  start-count
  max-count
  ;; sequences := (sequence ...)
  ;; sequence  := queue|dequeue
  ;; queue     := (:queue (<value> ...))
  ;; dequeue   := (:deq (<expected value> ...))
  sequences)

(defun is-list (lst1 lst2)
  (equal lst1 lst2))

(defun.ps-only is-list (lst1 lst2)
  (cond ((and (null lst1) (null lst2)) t)
        ((null lst1)
         (= (length lst2) 0))
        ((null lst2)
         (= (length lst1) 0))
        ((not (= (length lst1) (length lst2))) nil)
        (t (dotimes (i (length lst1))
             (unless (= (nth i lst1) (nth i lst2))
               (return-from is-list nil)))
           t)))

(deftest.ps+ test-buffered-queue
  (dolist (def (list (make-test-def
                      :desc "Don't pop until reaches to start-count"
                      :min-count   2
                      :start-count 4
                      :max-count   6
                      :sequences '((:deq ())    ; count 0
                                   (:queue (1 2 3)) ; count 3
                                   (:deq ())    ; count 3
                                   (:queue (4)) ; count 4
                                   (:deq (1))   ; count 3
                                   (:deq (2))   ; count 2
                                   (:queue (5)) ; count 3
                                   (:deq (3))   ; count 2
                                   ))
                     (make-test-def
                      :desc "Dequeue multiple when exceeds max-count"
                      :min-count   2
                      :start-count 4
                      :max-count   6
                      :sequences '((:queue (1 2 3 4 5 6)) ; count 6
                                   (:deq (1))     ; count 5
                                   (:queue (7 8)) ; count 7
                                   (:deq (2 3 4)) ; count 4
                                   ))
                     (make-test-def
                      :desc "Store again if is lower than min-count"
                      :min-count   2
                      :start-count 4
                      :max-count   6
                      :sequences '((:queue (1 2 3 4)) ; count 4
                                   (:deq (1)) ; count 3
                                   (:deq (2)) ; count 2
                                   (:deq (3)) ; count 1
                                   (:deq ())  ; count 1
                                   (:queue (5))   ; count 2
                                   (:deq ())      ; count 2
                                   (:queue (6 7)) ; count 4
                                   (:deq (4))))))
    (let ((queue (init-buffered-queue
                  :min-count   (test-def-min-count def)
                  :start-count (test-def-start-count def)
                  :max-count   (test-def-max-count def))))
      (testing (test-def-desc def)
        (dolist (seq (test-def-sequences def))
          (ecase (car seq)
            (:queue (queue-to-buffer queue (cadr seq)))
            (:deq (ok (is-list (dequeue-list-from-buffer queue)
                               (cadr seq))))))))))
