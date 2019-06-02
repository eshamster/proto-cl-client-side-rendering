(defpackage proto-cl-client-side-rendering/main
  (:nicknames :proto-cl-client-side-rendering)
  (:use :cl
        :proto-cl-client-side-rendering/game-loop
        :proto-cl-client-side-rendering/graphics
        :proto-cl-client-side-rendering/input
        :proto-cl-client-side-rendering/middleware
        :proto-cl-client-side-rendering/protocol
        :proto-cl-client-side-rendering/utils
        :proto-cl-client-side-rendering/ws-server
        :proto-cl-client-side-rendering/client-list-manager)
  (:export ;; game-loop
           :start-game-loop
           :stop-game-loop
           :log-console
           ;; graphics
           :draw-rect
           :draw-circle
           :draw-line
           :draw-arc
           ;; input
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
           :get-touch-summary-pos
           ;; middleware
           :make-client-side-rendering-middleware
           ;; protocol
           :send-draw-circle
           ;; utils
           :ensure-js-files
           :make-src-list-for-script-tag
           ;; ws-server
           :*target-client-id-list*
           :register-callback-on-connecting
           :register-callback-on-disconnecting
           ;; client-list-manager
           :get-new-client-id-list
           :get-deleted-client-id-list
           :get-client-id-list))
