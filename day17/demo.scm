
(cond-expand
  (chicken-4 (use (prefix sdl2 "sdl2:")
                  (prefix sdl2-ttf "ttf:")))
  (chicken-5 (import (prefix sdl2 "sdl2:")
                     (prefix sdl2-ttf "ttf:"))))

(sdl2:set-main-ready!)
(sdl2:init!)
(ttf:init!)



(define font (ttf:open-font "/home/terry/advent-of-code/2023/day16/ProggyClean.ttf" 40))
(define text "Hello, World!")
(define-values (w h) (ttf:size-utf8 font text))
(define window-width 800)
(define window-height 800)
(define window (sdl2:create-window! text 'centered 'centered window-width window-height))
(define window-surface (sdl2:window-surface window))
(define renderer (sdl2:get-renderer window))
(sdl2:render-draw-color-set! renderer (sdl2:make-color 255 255 255 255))
(sdl2:render-clear! renderer)

(let ((text-surf (ttf:render-utf8-shaded
                  font text
                  (sdl2:make-color 0   0   0)
                  (sdl2:make-color 255 255 255))))
  (sdl2:blit-surface! text-surf #f window-surface #f))

(sdl2:update-window-surface! window)

;;(render-target-set! renderer
(sdl2:render-draw-color-set! renderer (sdl2:make-color 255 0 0 255))
(sdl2:render-draw-line! renderer 0 0 200 200)
(sdl2:render-present! renderer)


(sdl2:delay! 5000)
(sdl2:quit!)


