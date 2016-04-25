#lang racket/gui
(require "block.rkt")
(require "board.rkt")
(require "game-init.rkt")
(require "cmd_store.rkt")

(define (my-proc)
  (send   ;fall

(define *fall-timer* (new timer%
                     [notify-callback my-proc]))

(send *fall-timer* start 1000 #f)

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