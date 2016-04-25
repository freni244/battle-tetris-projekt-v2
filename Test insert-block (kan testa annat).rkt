#lang racket

(define matrix (list (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 1 1 1 1 1 1 1 1 1 1)
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
                     (list 0 0 0 0 0 0 0 0 0 0)))

(define T '((4 1) (5 1) (6 1) (5 2)))

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

;;; Antingen denna:
;; Infogar ett blocks coordinater och färg/powerup (dvs nummer) 
(define (insert-block-alt1 block-color block-coord)
  (let ((part1 (first block-coord))  ;; de fyra delarnas coordinater
        (part2 (second block-coord))
        (part3 (third block-coord))
        (part4 (fourth block-coord)))
    (ins-to-matrix block-color (car part1) (cadr part1) matrix)
    (ins-to-matrix block-color (car part2) (cadr part2) matrix)
    (ins-to-matrix block-color (car part3) (cadr part3) matrix)
    (ins-to-matrix block-color (car part4) (cadr part4) matrix)))

(define (random-from-to n m)
  (+ n (random (- (+ m 1) n))))

;; eller denna:
(define (insert-block-alt2 block-coord)
      (let ((part1 (first block-coord))  ;; de fyra delarnas coordinater
            (part2 (second block-coord))
            (part3 (third block-coord))
            (part4 (fourth block-coord))
            (block-color (random-from-to 1 7))) ;; 1-7 representerar en färg ; (gör det här -> behöver ej block-color som variabel. ...)
        (ins-to-matrix block-color (car part1) (cadr part1) matrix)
        (ins-to-matrix block-color (car part2) (cadr part2) matrix)
        (ins-to-matrix block-color (car part3) (cadr part3) matrix)
        (ins-to-matrix block-color (car part4) (cadr part4) matrix)))