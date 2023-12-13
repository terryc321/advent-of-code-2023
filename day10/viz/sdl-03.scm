

;;(import sdl2)
(import simple-exceptions)



;; Compatible with both CHICKEN 4 and CHICKEN 5.
(cond-expand
 (chicken-4 (use (prefix sdl2 "sdl2:")))
 (chicken-5 (import (prefix sdl2 "sdl2:"))))

;; Schedule quit! to be automatically called when your program exits normally.
(on-exit sdl2:quit!)

;; Install a custom exception handler that will call quit! and then
;; call the original exception handler. This ensures that quit! will
;; be called even if an unhandled exception reaches the top level.
(current-exception-handler
 (let ((original-handler (current-exception-handler)))
   (lambda (exception)
     (sdl2:quit!)
     (original-handler exception))))



(sdl2:set-main-ready!)
(sdl2:init! '(video))
(define window (sdl2:create-window! "Hello, World!" 0 0 600 400))
(sdl2:fill-rect! (sdl2:window-surface window)
                 #f
                 (sdl2:make-color 0 128 255))

;;(sdl2:render-draw-line! 
#|
(sdl2:fill-rect! (sdl2:make-rect 0 0 100 100)
                 #f
                 (sdl2:make-color 255 0 0))
|#

;;(sdl2:rect 0 0 100 100)
(sdl2:update-window-surface! window)
(sdl2:delay! 3000)
(sdl2:quit!)





