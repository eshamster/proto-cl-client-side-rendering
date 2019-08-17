(defpackage sample-proto-cl-client-side-rendering/main
  (:nicknames :sample-proto-cl-client-side-rendering)
  (:use :cl
        :sample-proto-cl-client-side-rendering/server
        :sample-proto-cl-client-side-rendering/admin/admin)
  (:export :start
           :stop))
