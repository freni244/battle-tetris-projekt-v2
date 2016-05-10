#lang racket
(require "block.rkt")
(require "board.rkt")
(require "game-init.rkt")

;; Styrning av block (sidledes, ner & rotation) på ett spelbräde. Inget händer om coordinat ej occuperad eller utanför brädet. Inargument: board
(define (move key-event board)
  (let ((cur-block (send board get-cur-block))
        (occupied-coord (send board get-occupied-coord))
        (bottom (send board get-bottom))
        (left-wall (send board get-left-wall))
        (right-wall (send board get-right-wall))
        (left-key (send board get-left-key))
        (right-key (send board get-right-key))
        (down-key (send board get-down-key))
        (rotate-right-key (send board get-rotate-right-key))
        (rotate-left-key (send board get-rotate-left-key)))
    (cond
      [(equal? key-event left-key)
       (cond ((or (occurs-coordinates? (send cur-block return-move-direction 'left) occupied-coord) (occurs-coordinates? (send cur-block get-place) left-wall))
              void)
             (else (send cur-block move-direction 'left)))]
      [(equal? key-event right-key)
       (cond ((or (occurs-coordinates? (send cur-block return-move-direction 'right) occupied-coord) (occurs-coordinates? (send cur-block get-place) right-wall))
              void)
             (else (send cur-block move-direction 'right)))]
      [(equal? key-event down-key)
       (cond ((or (occurs-coordinates? (send cur-block return-move-down) occupied-coord) (occurs-coordinates? (send cur-block get-place) bottom))
              void)
             (else (send cur-block move-down)))]
      [(equal? key-event rotate-right-key)
       (send cur-block rotate 'right)]
      [(equal? key-event rotate-left-key)
       (send cur-block rotate 'left)]
       ;(cond ((or (occurs-coordinates? (send cur-block return-rotate 'right) occupied-coord) (occurs-coordinates? (send cur-block get-place) bottom)) ;; utöka med vägg osv
       ;       void)
       ;      (else (send cur-block rotate 'right)))]
      )))

;      [(equal? key-event #\a)
;       (cond ((or (occurs-coordinates? (send cur-block-b2 return-move-direction 'left) occupied-coord) (occurs-coordinates? (send cur-block-b2 get-place) left-wall))
;              void)
;             (else (send cur-block-b2 move-direction 'left)))]

(define (occurs? el list)
  (not (null? (filter (lambda (x) (equal? x el)) list))))

;; Kollar om samma koordinater finns i två listor med koordinater.
(define (occurs-coordinates? coord-lst-1 coord-lst-2)
  (cond ((or (null? coord-lst-1) (null? coord-lst-2)) #f)
        ((occurs? (car coord-lst-1) coord-lst-2) #t)
        (else (occurs-coordinates? (cdr coord-lst-1) coord-lst-2))))

(provide (all-defined-out))