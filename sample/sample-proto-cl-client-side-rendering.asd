#|
  This file is a part of proto-cl-client-side-rendering project.
  Copyright (c) 2019 eshamster (hamgoostar@gmail.com)
|#

#|
  A sample of proto-cl-client-side-rendering in Common Lisp

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem "sample-proto-cl-client-side-rendering"
  :version "0.1.0"
  :author "eshamster"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :license "MIT"
  :depends-on (:cl-markup
               :ningle
               :proto-cl-client-side-rendering
               :sample-proto-cl-client-side-rendering/main)
  :description "A sample of Client side rendering in Common Lisp")
