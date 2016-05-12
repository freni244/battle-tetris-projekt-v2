#lang racket
(require "block.rkt")
(require "board.rkt")
(require "game-init.rkt")

(define (rotate-cond items) ;Returnerar #t om ingen av coordinaterna i items är utanför spelplanen
  (if (empty? items)
      #t
      (if (and (< (car (car items)) 11) (> (car (car items)) 0) (< (car (cdr (car items))) 21) (> (car (cdr (car items))) 0))
          (rotate-cond (cdr items))
          #f)))

;; Styrning av block (sidledes, ner & rotation). Inget händer om coordinat ej occuperad eller utanför brädet.
(define (move key-event)
  (let ((cur-block (send *board-1* get-cur-block))
        (occupied-coord (send *board-1* get-occupied-coord))
        (bottom (send *board-1* get-bottom))
        (left-wall (send *board-1* get-left-wall))
        (right-wall (send *board-1* get-right-wall))
        (board-matrix (send *board-1* get-matrix)))
    (cond
      [(equal? key-event 'left) ;Flytta block vänster
       (cond ((or (occurs-coordinates? (send cur-block return-move-direction 'left) occupied-coord) (occurs-coordinates? (send cur-block get-place) left-wall))
              void)
             (else (send cur-block move-direction 'left)))]
      [(equal? key-event 'right);Flytta block höger
       (cond ((or (occurs-coordinates? (send cur-block return-move-direction 'right) occupied-coord) (occurs-coordinates? (send cur-block get-place) right-wall))
              void)
             (else (send cur-block move-direction 'right)))]
      [(equal? key-event 'down);Flytta block nedåt
       (cond ((or (occurs-coordinates? (send cur-block return-move-down) occupied-coord) (occurs-coordinates? (send cur-block get-place) bottom))
              void)
             (else (send cur-block move-down)))]
      [(equal? key-event #\space);Rotera block höger
       (cond ((and (rotate-cond (send cur-block return-rotate 'right)) (not (occurs-coordinates? (send cur-block return-rotate 'right) occupied-coord)))
              (send cur-block rotate 'right)))]
      [(equal? key-event 'shift);Rotera block vänster
       (cond ((and (rotate-cond (send cur-block return-rotate 'left)) (not (occurs-coordinates? (send cur-block return-rotate 'left) occupied-coord)))
              (send cur-block rotate 'left)))]
      )))

(define (occurs? el list)
  (not (null? (filter (lambda (x) (equal? x el)) list))))

;; Kollar om samma koordinater finns i två listor med koordinater.
(define (occurs-coordinates? coord-lst-1 coord-lst-2)
  (cond ((or (null? coord-lst-1) (null? coord-lst-2)) #f)
        ((occurs? (car coord-lst-1) coord-lst-2) #t)
        (else (occurs-coordinates? (cdr coord-lst-1) coord-lst-2))))

(provide (all-defined-out))