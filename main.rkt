#lang racket/gui
;(provide *draw-timer*)
(require "block.rkt")
(require "board.rkt")
(require "game-init.rkt")
(require "draw-game.rkt")
;(require "cmd_store.rkt")

; (send *draw-timer* start 1000 #f)

;; i loop:
;; tar bort rad om den är full (både i board-1 och 2)
;; kolla om "to high"
;; rita saker

;;; Vet inte om vi ska ha de här eller i draw...
(define *draw-timer* (new timer%
                     [notify-callback refresh-draw-cycle]))

(define *fall-timer* (new timer%
                     [notify-callback draw-fall]))

(define (game-loop)
  ;(let
  ;    ([])
  ;(send *draw-timer* start 60 #f)
  ;(send *fall-timer* start 300 #f)
  (cond ((send *board-1* too-high?)
         (display "You lose"))
        ;(( full-row
        (else (game-loop)
              ;(send *fall-timer* start 300 #f)
              )))

(define (play-game)
  (send *window* show #t)
;  (send *draw-timer* start 60 #f)
;  (send *fall-timer* start 300 #f)
  (game-loop))