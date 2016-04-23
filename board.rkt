#lang racket
(provide board%)
(require "block.rkt")

(define board%
  (class object%
    (init-field matrix)
    (field [next-blocks '()]
           [hold '()])
    
    ;; Inserts number to position.
    (define (ins-to-list el pos list)
      (let ((before (take list (- pos 1)))
            (after (drop list pos)))
        (append before (cons el after))))
    
    (define (select-n n list)
      (cond ((null? list) '())
            ((= n 1) (car list))
            (else (select-n (- n 1) (cdr list)))))
    
    ;; Inserts number to matrix.
    ;; Inarguments: inserted number "num", coordinates "x-pos, y-pos", matrix
    (define (ins-to-matrix num x-pos y-pos matrix)
      (let ((new-row (ins-to-list num x-pos (select-n y-pos matrix)))) ;; number inserted to row
        (ins-to-list new-row y-pos matrix)))
    
    (define/public (remove-row row-coord)
      (set! matrix (ins-to-list (list 0 0 0 0 0 0 0 0 0 0) row-coord matrix)))

    


    ))