#lang racket
(provide board%)
(require "block.rkt")

(define board%
  (class object%
    (init-field matrix
                left-key
                right-key
                down-key
                rotate-right-key
                rotate-left-key
                drop-key
                direction-keys) ;; (höger, vänster, ner)
    (field [next-blocks '()]
           [all-types '()]
           [score 0]
           [lost-game #f]
           [singelplayer #f]
           [active-keys '()])

    (define/public (get-active-keys) active-keys)

     ;; Lägger till en aktiv tangent om den inte finns i active-keys.
    (define/public (add-active-key key)
      (if (not (occurs? key active-keys))
          (set! active-keys (cons key active-keys))
          void))

    (define/public (remove-active-key key)
      (set! active-keys (filter (lambda (el) (and (not (equal? el key)) el)) active-keys)))

    (define/public (get-direction-keys) direction-keys)

    (define/public (direction-key? key)
      (occurs? key direction-keys))

    (define/public (get-score) score)

    (define/public (add-point point)
      (set! score (+ score point)))

    (define/public (lost-game?) lost-game) ;; Returnerar #t spelaren på spelbrädet har förlorat

    (define/public (lose)
      (set! lost-game #t))
    
    (define/public (play-singelplayer)
      (set! singelplayer #t))
    
    (define/public (singelplayer?) singelplayer)

    (define/public (get-matrix) matrix) ;; returnerar spelplanens matris
    
    ;; Returnerar knappar
    (define/public (get-left-key) left-key)
    (define/public (get-right-key) right-key)
    (define/public (get-down-key) down-key)
    (define/public (get-rotate-right-key) rotate-right-key)
    (define/public (get-rotate-left-key) rotate-left-key)
    (define/public (get-drop-key) drop-key)
    
    (define/public (get-bottom) ; (kunde hellre hämtats från game-init.)
      '((1 20) (2 20) (3 20) (4 20) (5 20) (6 20) (7 20) (8 20) (9 20) (10 20)))
    
    (define/public (get-left-wall)
      '((1 1) (1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8) (1 9) (1 10) (1 11) (1 12) (1 13) (1 14) (1 15) (1 16) (1 17) (1 18) (1 19) (1 20)))

    (define/public (get-right-wall)
      '((10 1) (10 2) (10 3) (10 4) (10 5) (10 6) (10 7) (10 8) (10 9) (10 10) (10 11) (10 12) (10 13) (10 14) (10 15) (10 16) (10 17) (10 18) (10 19) (10 20)))

    ;; Returnerar alla "typer" av block som finns på spelplanen som objekt.
    (define/public (get-all-types) all-types)
    
    (define/public (add-all-types type)
      (set! all-types (append (list type) all-types)))
    
    ;; Lägger block sist i listan next-blocks. Inargument: block (som objekt)
    (define/public (queue-block block)
      (set! next-blocks (append next-blocks (list block))))
    
    ;; Reutrnerar första blocket ur next-blocks.
    (define/public (get-cur-block)
      (if (null? next-blocks)
          (printf "error: there are no blocks left in next-blocks ")
          (car next-blocks)))
    
    ;; Tar bort första blocket ur next-blocks
    (define/public (remove-cur-block)
      (set! next-blocks (cdr next-blocks)))

    ;; Hämtar blocket som kommer efter cur-block
    (define/public (get-next-block)
      (if (null? next-blocks)
          (printf "error: there are no blocks left in next-blocks ")
          (car (cdr next-blocks))))
    
    (define/public (get-next-blocks) next-blocks)
    
    ;; Infogar element på position i lista.
    (define/public (ins-to-list el pos list)
      (let ((before (take list (- pos 1)))
            (after (drop list pos)))
        (begin (set! matrix (append before (cons el after))) matrix)))

    ;; Returnerar det n:e elementet i lista: n list -> "n:e element"
    (define/public (select-n n list)
      (cond ((null? list) '())
            ((= n 1) (car list))
            (else (select-n (- n 1) (cdr list)))))
    
    ;; Infogar nummer till matris.
    ;; Inargument: num x-pos y-pos matrix -> ny-matrix
    (define/public (ins-to-matrix num x-pos y-pos matrix)
      (let ((new-row (ins-to-list num x-pos (select-n y-pos matrix))))
        (ins-to-list new-row y-pos matrix)))

    ;; Infogar ett blocks färg (nummer 1-7) i spelbrädet vid blockets koordinater.
    ;; block-color block -> ny-matrix
    (define/public (insert-block block-color block)
      (ins-to-matrix block-color (send block get-x-part1) (send block get-y-part1) matrix)
      (ins-to-matrix block-color (send block get-x-part2) (send block get-y-part2) matrix)
      (ins-to-matrix block-color (send block get-x-part3) (send block get-y-part3) matrix)
      (ins-to-matrix block-color (send block get-x-part4) (send block get-y-part4) matrix))
    
    ;; Kollar om element finns i lista
    (define/public (occurs? el list)
      (not (null? (filter (lambda (x) (equal? x el)) list))))

    ;; Kollar om rad är full
    (define (full-row? row)
      (not (occurs? 0 row)))
    
    ;; Returnerar #t om det finns full rad i matrix, annars #f.
    (define/public (exist-full-row?)
      (let ((full-rows (filter full-row? matrix)))
        (not (null? full-rows))))

    ;; Sätter rad number y till en nollrad.
    ;; row-y (siffra) -> ny-matrix
    (define/public (remove-row row-y)
      (set! matrix (ins-to-list (list 0 0 0 0 0 0 0 0 0 0) row-y matrix)))

    ;; Räknar till full rad i matrix. -> #f eller rad-nummer
    (define/public (count-to-full-row)
      (define (count board y)
        (cond ((null? board) #f)
              ((full-row? (car board)) y)
              (else (count (cdr board) (+ y 1)))))
      (count matrix 1))
    
      (define/public (get-row-y y)
        (define (count-to-row board y)
          (cond ((null? board) #f)
                ((= y 1) (car board))
                (else (count-to-row (cdr board) (- y 1)))))
        (count-to-row matrix y))

      ;; Tar bort rad y och flyttar ner allt över.
      (define/public (collapse-from y)
        (define (build-matrix y new-matrix)
          (cond ((= y 1)
                 (set! matrix new-matrix))
                (else (build-matrix (- y 1) (ins-to-list (get-row-y (- y 1)) y new-matrix)))))
        (build-matrix y matrix))
    
    ;; Kollar om översta raden i en board-matrix har något annat än 0.
    ;; -> #t #f.
    (define/public (too-high?)
      (let ((all-but-zero (filter (lambda (x) (and (not (= 0 x)) x)) (car matrix))))
        (not (null? all-but-zero))))

    (define/public (row-of-zeros? row)
      (equal? '(0 0 0 0 0 0 0 0 0 0) row))

    ;; Skapar koordinatlista med y, samt de x koordinater som inte är noll ur row. Dvs returnerar "occuperade" koordinater (x>0).
    ;; Ex: y = 2 row '(1 0 3 0 4) -> '((1 2) (3 2) (4 2))
    (define/public (take-occ-coord y row)
      (define (build-lst x y x-lst xy-lst)
        (cond ((null? x-lst) xy-lst)
              ((not (= (car x-lst) 0))
               (build-lst (+ x 1) y (cdr x-lst) (cons (list x y) xy-lst)))
              (else (build-lst (+ x 1) y (cdr x-lst) xy-lst))))
      (build-lst 1 y row '()))
    
    ;; Returnerar de koordinater i matrix som är occuperade (värde > 0)
    (define/public (get-occupied-coord)
      (define (make-coord-list y board coord-list)
        (cond ((null? board) coord-list)
              ((not (row-of-zeros? (car board)))
               (make-coord-list (+ y 1) (cdr board) (append (take-occ-coord y (car board)) coord-list)))
              (else (make-coord-list (+ y 1) (cdr board) coord-list))))
      (make-coord-list 1 matrix '()))
      
    (super-new)))