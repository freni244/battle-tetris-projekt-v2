#lang racket
(provide board%)
;(require "block.rkt")
;(require "game-init.rkt")

(define board%
  (class object%
    (init-field matrix)
    (field [next-blocks '()]
           [hold '()])

    (define/public (get-matrix) matrix)

    ;; Lägger block sist i listan next-blocks. Inargument: block (som objekt)
    (define/public (add-next-block block)
      (set! next-blocks (append next-blocks (list block))))

    ;; Reutrnerar och tar bort första blocket ur next-blocks.
    (define/public (get-cur-block)
      (if (null? next-blocks)
          #f
          (begin (car next-blocks)
                 (set! next-blocks (cdr next-blocks)))))

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
      (not (null? (filter (lambda (x) (equal? x el)) list))))

    ;; Kollar om rad är full
    (define/public (full-row? row)
      (not (occurs? 0 row)))

    ;; Tar bort första fulla raden ur en board-matris.
    (define/public (remove-full-row board)
      (define (count-to-full-row board y)
        (cond ((null? board) #f)
              ((full-row? (car board)) y)
              (else (count-to-full-row (cdr board) (+ y 1)))))
      (let ((full-row-y (count-to-full-row board 1)))
        (if (not (equal? full-row-y #f))
            (remove-row full-row-y)
            #f)))
    
    ;; Kollar om första raden i en board-matrix har något annat än 0. Inargument: board. Returnerar #t #f.
    (define/public (too-high? board)
      (let ((all-but-zero (filter (lambda (x) (and (not (= 0 x)) x)) (car board))))
        (not (null? all-but-zero))))

    (define/public (row-of-zeros? row)
      (equal? '(0 0 0 0 0 0 0 0 0 0) row))

    (define/public (take-occ-coord y x-lst)
      (define (build-lst x y x-lst xy-lst)
        (cond ((null? x-lst) xy-lst)
              ((not (= (car x-lst) 0))
               (build-lst (+ x 1) y (cdr x-lst) (cons (list x y) xy-lst)))
              (else (build-lst (+ x 1) y (cdr x-lst) xy-lst))))
      (build-lst 1 y x-lst '()))
    
    ;; Returnerar de koordinater i matrix som är occuperade (värde > 0)
    (define/public (get-occupied-coord)
      (define (make-coord-list y board coord-list)
        (cond ((null? board) coord-list)
              ((not (row-of-zeros? (car board)))
               (make-coord-list (+ y 1) (cdr board) (append (take-occ-coord y (car board)) coord-list)))
              (else (make-coord-list (+ y 1) (cdr board) coord-list))))
      (make-coord-list 1 matrix '()))


    
    ;; skapar 
;    (define/public (create-coord-lst y x-lst)
;      (map (lambda (x) (list x y)) x-lst))
;    
;    (define/public (occupied-coord) ;; board som argument när lägger till fler boards...
;      (define (make-coord-list y board coord-list)
;        (let ((all-but-zero (filter (lambda (x) (and (not (= 0 x)) x)) (car board))))
;          (cond ((null? board) coord-list)
;                ((not (row-of-zeros? (car board)))
;                 (make-coord-list (+ y 1) (cdr board) (append (create-coord-lst y all-but-zero) coord-list)))
;                (else (make-coord-list (+ y 1) (cdr board) coord-list)))))
;      (make-coord-list 1 matrix '()))
      
    
    (define/public (check-powerup row)
      (occurs? 8 row)) ;; beroende på vilket nummer vi ska ha för powerup
    
    
 ;   (define/public (activate-powerup row)
 ;     ())
      
    (super-new)))