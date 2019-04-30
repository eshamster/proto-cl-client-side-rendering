(defpackage proto-cl-client-side-rendering/main
  (:nicknames :proto-cl-client-side-rendering)
  (:use :cl
        :proto-cl-client-side-rendering/game-loop
        :proto-cl-client-side-rendering/middleware
        :proto-cl-client-side-rendering/protocol
        :proto-cl-client-side-rendering/utils)
  (:export :start-game-loop
           :stop-game-loop
           :draw-circle

           :make-client-side-rendering-middleware

           :send-draw-circle

           :ensure-js-files
           :make-src-list-for-script-tag))
