(defpackage proto-cl-client-side-rendering/t/utils/input
  (:use :cl
        :rove
        :proto-cl-client-side-rendering/utils/input))
(in-package :proto-cl-client-side-rendering/t/utils/input)

(defstruct test-def
  desc
  ;; (; frame1
  ;;  ((key . down-p) (key . down-p)...)
  ;;  ; frame2
  ;;  ((key . down-p) (key . down-p)...)...)
  input
  ;; (; frame1
  ;;  ((key . state) (key . state)...)
  ;;  ; frame2
  ;;  ((key . state) (key . state)...)...)
  expected)

(defun state-p (info key expected)
  (ecase expected
    (:down-now (input-down-now-p info key))
    (:down     (input-down-p     info key))
    (:up-now   (input-up-now-p   info key))
    (:up       (input-up-p       info key))))

(deftest input
  (dolist (def (list (make-test-def
                      :desc "simple key process"
                      :input '(((:a . t))
                               ()
                               ()
                               ((:a . nil))
                               ()
                               ())
                      :expected '(((:a . :down-now))
                                  ((:a . :down))
                                  ((:a . :down))
                                  ((:a . :up-now))
                                  ((:a . :up))
                                  ((:a . :up))))
                     (make-test-def
                      :desc "multiple key"
                      :input '(((:a . t) (:b . t))
                               ()
                               ((:b . nil))
                               ())
                      :expected '(((:a . :down-now) (:b . :down-now))
                                  ((:a . :down)     (:b . :down))
                                  ((:a . :down)     (:b . :up-now))
                                  ((:a . :down)     (:b . :up))))
                     (make-test-def
                      :desc "empty input"
                      :input '(())
                      :expected '(((:a . :up))))
                     (make-test-def
                      :desc "Press and release in same frame"
                      :input '(((:a . t) (:a . nil))
                               ((:a . t))
                               (()))
                      :expected '(((:a . :down-now))
                                  ((:a . :down-now))
                                  ((:a . :up-now))))))
    (testing (test-def-desc def)
      (let ((info (make-key-input-info)))
        (loop
           :for input    :in (test-def-input    def)
           :for expected :in (test-def-expected def)
           :do (progn
                 (dolist (each-input input)
                   (let ((key (car each-input))
                         (down-p (cdr each-input)))
                     (set-raw-key-state info key down-p)))
                 (update-key-input-info info)
                 (dolist (each-ex expected)
                   (let ((key (car each-ex))
                         (state (cdr each-ex)))
                     (ok (state-p info key state))))))))))
