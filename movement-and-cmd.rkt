#lang racket
;(require "block.rkt")
;(require "board.rkt")
(require "game-init.rkt")


(define (occurs? el list)
  (not (null? (filter (lambda (x) (equal? x el)) list))))

; Kollar om samma koordinater finns i två listor med koordinater.
(define (occurs-coordinates? coord-lst-1 coord-lst-2)
  (cond ((or (null? coord-lst-1) (null? coord-lst-2)) #f)
        ((occurs? (car coord-lst-1) coord-lst-2) #t)
        (else (occurs-coordinates? (cdr coord-lst-1) coord-lst-2))));

(define (fall cur-block);Ska skicka blocket ned i botten.
  (let ((block-coord '(1 20))
        (bottom '((1 20) (2 20) (3 20) (4 20) (5 20) (6 20) (7 20) (8 20) (9 20) (10 20))))
    (cond ((or (occurs-coordinates? block-coord '((1 7) (2 7) (3 7) (4 7) (5 7) (6 7))) (occurs-coordinates? block-coord bottom)) ;; ska komma från get-occupied-coord ;(send *board-1* get-occupied-coord)
           (printf "Sätt in blocket i board och skapa nytt block"))   ;; ta fram koordinaterna där matrisen har n > 0
          (else "bla")))) ; (move-down)))))


;(send *board-1* get-occupied-coord)