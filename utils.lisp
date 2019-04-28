(defpackage proto-cl-client-side-rendering/utils
  (:use :cl)
  (:export :ensure-js-files
           :make-src-list-for-script-tag))
(in-package :proto-cl-client-side-rendering/utils)

(defparameter *cdns*
  '("https://cdnjs.cloudflare.com/ajax/libs/three.js/104/three.js"))

(defparameter *js-pairs*
  '(("threex.keyboardstate.js" . "https://raw.githubusercontent.com/jeromeetienne/threex.keyboardstate/51fd77fdd87eeed064db643693d393cf21afa45d/threex.keyboardstate.js")))

(defun ensure-js-files (dir)
  (ensure-directories-exist dir)
  (dolist (pair *js-pairs*)
    (let ((path (merge-pathnames (car pair) dir))
          (url (cdr pair)))
      (unless (probe-file path)
        (with-open-file (file path
                              :direction :output
                              :if-exists :error
                              :if-does-not-exist :create)
          (format *error-output* "Download: ~A" (car pair))
          (princ (dex:get url) file))))))

(defun make-src-list-for-script-tag (relative-path)
  (append *cdns*
          (remove-if (lambda (path)
                       (not (string= (pathname-type path) "js")))
                     (mapcar (lambda (pair)
                               (merge-pathnames (car pair) relative-path))
                             *js-pairs*))))
