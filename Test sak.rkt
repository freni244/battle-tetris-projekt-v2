#lang racket/gui
(require "block.rkt")
(require "board.rkt")
(require "game-init.rkt")

(define T-test '((4 1) (5 1) (6 1) (5 2)))

(define matrix (list (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 15 0 0 0 0 0 0 0 0 0)
                     (list 16 1 0 1 0 1 1 0 1 0)
                     (list 17 1 1 1 0 1 1 0 1 0)
                     (list 18 1 2 1 1 1 1 2 1 1)
                     (list 19 1 1 1 0 1 1 0 1 0)
                     (list 20 1 1 1 1 1 1 0 0 0)))

;; Infogar nummer på position.
    (define (ins-to-list el pos list)
      (let ((before (take list (- pos 1)))
            (after (drop list pos)))
        (begin (set! matrix (append before (cons el after))) matrix)))
    
    (define (select-n n list)
      (cond ((null? list) '())
            ((= n 1) (car list))
            (else (select-n (- n 1) (cdr list)))))
    
    ;; Infogar nummer till matris.
    ;; Inargument: infogat nummer "num", koordinater "x-pos, y-pos", matrix
    (define (ins-to-matrix num x-pos y-pos matrix)
      (let ((new-row (ins-to-list num x-pos (select-n y-pos matrix)))) ;; number inserted to row
        (ins-to-list new-row y-pos matrix)))
    
    ;; Infogar ett blocks coordinater och färg/powerup (dvs nummer) 
    (define (insert-block block-color block-coord) ;; två variabler verkar inte funka vid "send"...
      (let ((part1 (first block-coord))  ;; de fyra delarnas coordinater
            (part2 (second block-coord))
            (part3 (third block-coord))
            (part4 (fourth block-coord)))
        (ins-to-matrix block-color (car part1) (cadr part1) matrix)
        (ins-to-matrix block-color (car part2) (cadr part2) matrix)
        (ins-to-matrix block-color (car part3) (cadr part3) matrix)
        (ins-to-matrix block-color (car part4) (cadr part4) matrix)))

    (define (insert-block-2 block-color block)
      (ins-to-matrix block-color (send block get-x-part1) (send block get-y-part1) matrix)
      (ins-to-matrix block-color (send block get-x-part2) (send block get-y-part2) matrix)
      (ins-to-matrix block-color (send block get-x-part3) (send block get-y-part3) matrix)
      (ins-to-matrix block-color (send block get-x-part4) (send block get-y-part4) matrix))

    ;; Tar bort block. Sätter nollor vid blockets coordinater. Inargument: blockets coordinater
    (define (remove-block block-coord)
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
    
    ;; Kollar om element finns i lista
    (define (occurs? el list)
      (not (null? (filter (lambda (x) (equal? x el)) list))))

    ;; Kollar om rad är full
    (define (full-row? row)
      (not (occurs? 0 row)))
    
    ;; Returnerar #t om det finns full rad i matrix, annars #f.
    (define (exist-full-row?)
      (let ((full-rows (filter full-row? matrix)))
        (not (null? full-rows))))

    ;; Sätter rad number y till en nollrad. Inargument: row-y (siffra)
    (define (remove-row row-y)
      (set! matrix (ins-to-list (list 0 0 0 0 0 0 0 0 0 0) row-y matrix)))

    ;; Tar bort första fulla raden ur en board-matris.
    (define (remove-full-row)
      (define (count-to-full-row board y)
        (cond ((null? board) #f)
              ((full-row? (car board)) y)
              (else (count-to-full-row (cdr board) (+ y 1)))))
      (let ((full-row-y (count-to-full-row matrix 1)))
        (if (not (equal? full-row-y #f))
            (remove-row full-row-y)
            ;(begin (remove-row full-row-y)
            ;      ) ;; flytta ner allt över
            #f)))

      (define (get-row-y y)
        (define (count-to-full-row board y)
          (cond ((null? board) #f)
                ((= y 1) (car board))
                (else (count-to-full-row (cdr board) (- y 1)))))
        (count-to-full-row matrix y))
      
      ;; Flyttar ner allt över row, ett steg.
      (define (collapse-from y)
        (define (build-matrix y new-matrix)
          (cond ((= y 1)
                 (set! matrix new-matrix))
                (else (build-matrix (- y 1) (ins-to-list (get-row-y (- y 1)) y new-matrix)))))
        (build-matrix y matrix))

(define (count-to-full-row)
      (define (count board y)
        (cond ((null? board) #f)
              ((full-row? (car board)) y)
              (else (count (cdr board) (+ y 1)))))
      (count matrix 1))



(define shift-focus
  (let ((key-focus 'b1))
    (lambda ()
      (cond ((equal? key-focus 'b1)
             (set! key-focus 'b2)
             (display key-focus))
             ;(send *game-canvas-b1* focus))
            (else
             (set! key-focus 'b1)
             (display key-focus))))))
         

(define *shift-focus-timer* (new timer%
                                 [notify-callback shift-focus]))

;(send *shift-focus-timer* start 1000 #f)
