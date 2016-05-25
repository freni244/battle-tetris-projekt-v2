#lang racket
(require "block.rkt")
(require "board.rkt")
(require "game-init.rkt")

;; Förekommer element i lista? -> #t #f
(define (occurs? el list)
  (not (null? (filter (lambda (x) (equal? x el)) list))))

;; Kollar om samma koordinater finns i två listor med koordinater.
(define (occurs-coordinates? coord-lst-1 coord-lst-2)
  (cond ((or (null? coord-lst-1) (null? coord-lst-2)) #f)
        ((occurs? (car coord-lst-1) coord-lst-2) #t)
        (else (occurs-coordinates? (cdr coord-lst-1) coord-lst-2))))

(define (rotate-cond items) ;Returnerar #t om ingen av coordinaterna i items är utanför spelplanen
  (if (empty? items)
      #t
      (if (and (< (car (car items)) 11) (> (car (car items)) 0) (< (car (cdr (car items))) 21) (> (car (cdr (car items))) 0))
          (rotate-cond (cdr items))
          #f)))

;; Anropar move på varje element i en lista.
(define (key-list-to-move key-list board)
  (map (lambda (key) (move key board)) key-list))

;; Styrning av block (sidledes, ner och rotation) på ett spelbräde. Inget händer om coordinaten redan är occuperad eller utanför brädet.
;; Inargument: key-event och board
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
        (rotate-left-key (send board get-rotate-left-key))
        (drop-key (send board get-drop-key)))

    (define (move-to-bot);Hjälpfunktion till key-event 'control.
      (if (or (occurs-coordinates? (send cur-block return-move-down) occupied-coord) (occurs-coordinates? (send cur-block get-place) bottom))
          void
          (begin (send cur-block move-down)
                 (move-to-bot))))    
    (cond
      [(equal? key-event left-key) ;; Flytta block vänster
       (cond ((or (occurs-coordinates? (send cur-block return-move-direction 'left) occupied-coord) (occurs-coordinates? (send cur-block get-place) left-wall))
              void)
             (else (send cur-block move-direction 'left)))]
      [(equal? key-event right-key) ;; Flytta block höger
       (cond ((or (occurs-coordinates? (send cur-block return-move-direction 'right) occupied-coord) (occurs-coordinates? (send cur-block get-place) right-wall))
              void)
             (else (send cur-block move-direction 'right)))]
      [(equal? key-event down-key) ;; Flytta block nedåt
       (cond ((or (occurs-coordinates? (send cur-block return-move-down) occupied-coord) (occurs-coordinates? (send cur-block get-place) bottom))
              void)
             (else (send cur-block move-down)))]
      [(equal? key-event rotate-right-key) ;; Rotera block höger
       (cond ((and (rotate-cond (send cur-block return-rotate 'right)) (not (occurs-coordinates? (send cur-block return-rotate 'right) occupied-coord)))
              (send cur-block rotate 'right)))]
      [(equal? key-event rotate-left-key) ;; Rotera block vänster
       (cond ((and (rotate-cond (send cur-block return-rotate 'left)) (not (occurs-coordinates? (send cur-block return-rotate 'left) occupied-coord)))
              (send cur-block rotate 'left)))]
      [(equal? key-event drop-key) ;; Skickar ned ett block så långt som möjligt
       (move-to-bot)])))

(provide (all-defined-out))