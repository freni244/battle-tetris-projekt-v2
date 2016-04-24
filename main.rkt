#lang racket/gui
(require "block.rkt")
(require "board.rkt")
(require "game-init.rkt")
(require "cmd_store.rkt")

(define *window* (new frame%
                     [label "window"]
                     [width 900]
                     [height 600]
                     [x 0]	 
                     [y 0]))
(send *window* show #t)

;; i loop:
;; tar bort rad om den är full (både i board-1 och 2)
;; kolla om "to high"
;; rita saker

;(define (game-loop)
;  (let
;      ([])
;    (cond
;      )))
;(define (play-game)
;  (send *window* show #t)
;  (game-loop))