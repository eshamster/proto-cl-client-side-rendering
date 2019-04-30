(defpackage proto-cl-client-side-rendering/main
  (:nicknames :proto-cl-client-side-rendering)
  (:use :cl
        :proto-cl-client-side-rendering/game-loop
        :proto-cl-client-side-rendering/ws-server
        :proto-cl-client-side-rendering/server
        :proto-cl-client-side-rendering/middleware)
  (:export :start
           :stop

           :send-from-server

           :make-client-side-rendering-middleware))
