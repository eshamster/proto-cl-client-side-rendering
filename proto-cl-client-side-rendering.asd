#|
  This file is a part of proto-cl-client-side-rendering project.
  Copyright (c) 2019 eshamster (hamgoostar@gmail.com)
|#

#|
  A sample of WebSocket in Common Lisp

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem "proto-cl-client-side-rendering"
  :version "0.1.0"
  :author "eshamster"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :license "MIT"
  :depends-on (:websocket-driver-server
               :websocket-driver-client
               :alexandria
               :bordeaux-threads
               :lack-middleware-static
               :clack
               :dexador
               :cl-markup
               :cl-ppcre
               :jonathan
               :parenscript
               :ps-experiment
               :proto-cl-client-side-rendering/main)
  :description "A sample of Client side rendering in Common Lisp"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "proto-cl-client-side-rendering-test"))))
