#lang racket
(require "block.rkt")
(require "board.rkt")
(require "game-init.rkt")

;(define (legal-move? coord)
;  (

;; Styrning av block (sidledes, ner & rotation). Inget händer om coordinat ej occuperad eller utanför brädet.
(define (move key-event)
  (let ((cur-block (send *board-1* get-cur-block))
        (occupied-coord (send *board-1* get-occupied-coord))
        (bottom (send *board-1* get-bottom))
        (left-wall (send *board-1* get-left-wall))
        (right-wall (send *board-1* get-right-wall)))
    (cond
      [(equal? key-event 'left)
       (cond ((or (occurs-coordinates? (send cur-block return-move-direction 'left) occupied-coord) (occurs-coordinates? (send cur-block get-place) left-wall))
              void)
             (else (send cur-block move-direction 'left)))]
      [(equal? key-event 'right)
       (cond ((or (occurs-coordinates? (send cur-block return-move-direction 'right) occupied-coord) (occurs-coordinates? (send cur-block get-place) right-wall))
              void)
             (else (send cur-block move-direction 'right)))]
      [(equal? key-event 'down)
       (cond ((or (occurs-coordinates? (send cur-block return-move-down) occupied-coord) (occurs-coordinates? (send cur-block get-place) bottom))
              void)
             (else (send cur-block move-down)))]
      ;[(equal? key-event #\space) (send cur-block rotate)]
      )))

(define (occurs? el list)
  (not (null? (filter (lambda (x) (equal? x el)) list))))

;; Kollar om samma koordinater finns i två listor med koordinater.
(define (occurs-coordinates? coord-lst-1 coord-lst-2)
  (cond ((or (null? coord-lst-1) (null? coord-lst-2)) #f)
        ((occurs? (car coord-lst-1) coord-lst-2) #t)
        (else (occurs-coordinates? (cdr coord-lst-1) coord-lst-2))))

(provide (all-defined-out))