#lang racket/gui
;(provide *draw-timer*)
(require "block.rkt")
(require "board.rkt")
(require "game-init.rkt")
(require "draw-game.rkt")

;; Ändrar spelet om villkor är uppfyllda. (tex tar bort fulla rader och kollar om block är i toppen).
(define (conditions)  ;; istället för game-loop...
  (let
      ([full-row-pos-b1 (send *board-1* count-to-full-row)] ;; positionen av en full rad (board-1 och -2).
       [full-row-pos-b2 (send *board-2* count-to-full-row)])
    
    (cond [(send *board-1* too-high?)
           (send *fall-timer-b1* stop)]
          [(send *board-2* too-high?)
           (send *fall-timer-b2* stop)]
          
          [(send *board-1* exist-full-row?) ;; tar bort fulla rader och får blocken över att ramla.
           (send *board-1* collapse-from full-row-pos-b1)]
          [(send *board-2* exist-full-row?) ;; tar bort fulla rader och får blocken över att ramla.
           (send *board-2* collapse-from full-row-pos-b2)]
          
          [else void])))


(define *draw-timer* (new timer%
                     [notify-callback refresh-draw-cycle]))

(define *fall-timer-b1* (new timer%
                             [notify-callback (lambda ()
                                                (draw-fall *board-1*))]))

(define *fall-timer-b2* (new timer%
                             [notify-callback (lambda ()
                                                (draw-fall *board-2*))]))

(define *condition-timer* (new timer%
                               [notify-callback conditions]))

(define (play-game)
  (send *window* show #t)
  (send *draw-timer* start 16 #f)
  (send *fall-timer-b1* start 300 #f) ;; startar board-1
  (send *fall-timer-b2* start 300 #f) ;; startar board-2
  (send *condition-timer* start 16 #f)
  )