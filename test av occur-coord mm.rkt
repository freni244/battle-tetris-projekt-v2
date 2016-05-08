#lang racket
(define matrix
  (list (list 1 2 0 3 0 4 0 0 8 0)
        (list 0 0 0 0 0 0 0 0 0 0)
        (list 0 0 0 0 0 0 0 0 0 0)
        (list 0 0 0 0 0 0 0 0 0 0)
        (list 0 0 0 0 0 0 0 0 0 0)
        (list 0 2 0 0 0 0 0 0 0 0)
        (list 1 1 1 1 1 1 1 1 1 0)
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

  (define (row-of-zeros? row)
    (equal? '(0 0 0 0 0 0 0 0 0 0) row))

    ;; skapar 
    ;(define (create-coord-lst y x-lst)
    ;  (map (lambda (x) (list x y)) x-lst))

;; skapar en lista av ockuperade koordinater på "rad nummer y". Inargument: y koordinat, radens x-coordinater (x-lst)
(define (take-occ-coord y x-lst)
  (define (build-lst x y x-lst xy-lst)
    (cond ((null? x-lst) xy-lst)
          ((not (= (car x-lst) 0))
           (build-lst (+ x 1) y (cdr x-lst) (cons (list x y) xy-lst)))
          (else (build-lst (+ x 1) y (cdr x-lst) xy-lst))))
  (build-lst 1 y x-lst '()))
          

  (define (occupied-coord matrix) ;; board som argument när lägger till fler boards...
    (define (make-coord-list y board coord-list)
        (cond ((null? board) coord-list)
              ((not (row-of-zeros? (car board)))
               (make-coord-list (+ y 1) (cdr board) (append (take-occ-coord y (car board)) coord-list)))
              (else (make-coord-list (+ y 1) (cdr board) coord-list))))
    (make-coord-list 1 matrix '()))

    (define (occurs? el list)
      (not (null? (filter (lambda (x) (equal? x el)) list))))

    (define (occurs-coordinates? coord-lst-1 coord-lst-2)
      (cond ((or (null? coord-lst-1) (null? coord-lst-2)) #f)
            ((occurs? (car coord-lst-1) coord-lst-2) #t)
            (else (occurs-coordinates? (cdr coord-lst-1) coord-lst-2))))

(define block-coordinates '((5 6) (6 6) (6 7) (7 6)))

;testet:
; (occurs-coordinates? block-coordinates (occupied-coord matrix))

(define color 0)


(define (get-color-name) ;; returns name of color
  (cond ((= color 1) "lime")
        ((= color 2) "blue")
        ((= color 3) "red")
        ((= color 4) "yellow")
        ((= color 5) "orange")
        ((= color 6) "cyan")
        ((= color 7) "magenta")
        (else "gold")))