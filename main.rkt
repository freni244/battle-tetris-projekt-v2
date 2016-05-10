#lang racket/gui
;(provide *draw-timer*)
(require "block.rkt")
(require "board.rkt")
(require "game-init.rkt")
(require "draw-game.rkt")

;; Ändrar spelet om villkor är uppfyllda. (tex tar bort fulla rader och kollar om block är i toppen).
(define (conditions)  ;; istället för game-loop...
  (let
      ([full-row-pos (send *board-1* count-to-full-row)])
    
    (cond [(send *board-1* too-high?)
           (send *fall-timer* stop)
           (send *condition-timer* stop)]
          
          [(send *board-1* exist-full-row?)
           ;(send *board-1* remove-full-row) ;;;;;; behövs nog inte då collapse-from kan ersätta
           (send *board-1* collapse-from full-row-pos)]
          [else void])))

;;; Vet inte om vi ska ha de här eller i draw...
(define *draw-timer* (new timer%
                     [notify-callback refresh-draw-cycle]))

(define *fall-timer* (new timer%
                     [notify-callback draw-fall]))

(define *condition-timer* (new timer%
                               [notify-callback conditions]))

(define (play-game)
  (send *window* show #t)
  (send *draw-timer* start 16 #f)
  (send *fall-timer* start 700 #f)
  (send *condition-timer* start 16 #f)
  )