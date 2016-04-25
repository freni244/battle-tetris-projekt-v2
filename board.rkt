#lang racket
(provide board%)
;(require "block.rkt")

(define board%
  (class object%
    (init-field matrix)
    (field [next-blocks '()]
           [hold '()])

    (define/public (get-matrix) matrix)

    (define/public (get-next-blocks) next-blocks)

    (define/public (get-hold) hold)
    
    ;; Infogar nummer på position.
    (define/public (ins-to-list el pos list)
      (let ((before (take list (- pos 1)))
            (after (drop list pos)))
        (begin (set! matrix (append before (cons el after))) matrix)))
    
    (define/public (select-n n list)
      (cond ((null? list) '())
            ((= n 1) (car list))
            (else (select-n (- n 1) (cdr list)))))
    
    ;; Infogar nummer till matris.
    ;; Inargument: infogat nummer "num", koordinater "x-pos, y-pos", matrix
    (define/public (ins-to-matrix num x-pos y-pos matrix)
      (let ((new-row (ins-to-list num x-pos (select-n y-pos matrix)))) ;; number inserted to row
        (ins-to-list new-row y-pos matrix)))
    
    ;; Infogar ett blocks coordinater och färg/powerup (dvs nummer) 
    (define/public (insert-block block-color block-coord) ;; två variabler verkar inte funka vid "send"...
      (let ((part1 (first block-coord))  ;; de fyra delarnas coordinater
            (part2 (second block-coord))
            (part3 (third block-coord))
            (part4 (fourth block-coord)))
        (ins-to-matrix block-color (car part1) (cadr part1) matrix)
        (ins-to-matrix block-color (car part2) (cadr part2) matrix)
        (ins-to-matrix block-color (car part3) (cadr part3) matrix)
        (ins-to-matrix block-color (car part4) (cadr part4) matrix)))

    ;; Tar bort block. Sätter nollor vid blockets coordinater. Inargument: blockets coordinater
    (define/public (remove-block block-coord)
      (let ((part1 (first block-coord))  ;; de fyra delarnas coordinater
            (part2 (second block-coord))
            (part3 (third block-coord))
            (part4 (fourth block-coord)))
        (ins-to-matrix 0 (car part1) (cadr part1) matrix)
        (ins-to-matrix 0 (car part2) (cadr part2) matrix)
        (ins-to-matrix 0 (car part3) (cadr part3) matrix)
        (ins-to-matrix 0 (car part4) (cadr part4) matrix)))

      
;    (define (random-from-to n m)
;        (+ n (random (- (+ m 1) n))))
;    (let (block-color (random-from-to 1 7)))

    ;; Sätter rad number y till en nollrad. Inargument: row-y (siffra)
    (define/public (remove-row row-y)
      (set! matrix (ins-to-list (list 0 0 0 0 0 0 0 0 0 0) row-y matrix)))
    
    ;; Kollar om element finns i lista
    (define/public (occurs? el list)
      (not (null? (filter (lambda (x) (eq? x el)) list))))

    ;; Kollar om rad är full
    (define/public (check-row-full row)
      (not (occurs? 0 row)))

    ;; Tar bort första fulla raden ur en board-matris.
    (define/public (remove-full-row board)
      (define (count-to-full-row board y)
        (cond ((null? board) #f)
              ((check-row-full (car board)) y)
              (else (count-to-full-row (cdr board) (+ y 1)))))
      (let ((full-row-y (count-to-full-row board 1)))
        (if (not (equal? full-row-y #f))
            (remove-row full-row-y)
            #f)))

    ;; Kollar om första raden i en board-matrix har något annat än 0. Inargument: board. Returnerar #t #f.
    (define/public (check-to-high board)
      (let ((all-but-zero (filter (lambda (x) (and (not (= 0 x)) x)) (car board))))
        (not (null? all-but-zero))))
    
    (define/public (check-powerup row)
      (occurs? 8 row)) ;; beroende på vilket nummer vi ska ha för powerup
    
    
 ;   (define/public (activate-powerup row)
 ;     ())
      
    (super-new)))