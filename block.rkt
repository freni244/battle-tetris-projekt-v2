#lang racket/gui ;; ta bort gui
(provide block%)
;(require "board.rkt")
;(require "game-init.rkt")

(define block%
  (class object%
    (init-field coordinates
                type
                color)
    (field [place '()]
           [hold '()]
           [rotation 'up])
           ;[color 0]) ;; färgen är en slumpad siffra mellan 1-7. (kommer att inkludera powerup sen)

    
    (define/public (get-color) color)

    ;(define/public (create-block) 
    
;    (define/public (occurs? el list)
;      (not (null? (filter (lambda (x) (equal? x el)) list))))
;
;    ; Kollar om samma koordinater finns i två listor med koordinater.
;    (define/public (occurs-coordinates? coord-lst-1 coord-lst-2)
;      (cond ((or (null? coord-lst-1) (null? coord-lst-2)) #f)
;            ((occurs? (car coord-lst-1) coord-lst-2) #t)
;            (else (occurs-coordinates? (cdr coord-lst-1) coord-lst-2))));
;   
;    (define/public (fall);Ska skicka blocket ned i botten.
;      (let ((block-coord coordinates)
;            (bottom '((1 20) (2 20) (3 20) (4 20) (5 20) (6 20) (7 20) (8 20) (9 20) (10 20))))
;      (cond ((or (occurs-coordinates? block-coord '((1 7) (2 7) (3 7) (4 7) (5 7) (6 7))) (occurs-coordinates? block-coord bottom)) ;; ska komma från get-occupied-coord ;(send *board-1* get-occupied-coord)
;             (printf "Sätt in blocket i board och skapa nytt block"))   ;; ta fram koordinaterna där matrisen har n > 0
;            (else (move-down)))))

    (define/public (get-coord n xy items);Hämtar x/y koordinaten på n:e plats i listan.
      (if (= n 1)
          (if (eq? xy 'x)
              (caar items)
              (cadar items))
          (get-coord (- n 1) xy (cdr items))))

    ;adderar amount till koordinaten, kan vara negativ.
    (define/public (move-coords amount1 amount2 amount3 amount4 amount5 amount6 amount7 amount8);ändrar koordinaterna mha proc och amount,proc + eller -, amount hur mycket den ska flyttas.
      (set! coordinates (list (list (+ (get-coord 1 'x coordinates) amount1) (+ (get-coord 1 'y coordinates) amount2));Första koordinaten
                              (list (+ (get-coord 2 'x coordinates) amount3) (+ (get-coord 2 'y coordinates) amount4));Andra koordinaten
                              (list (+ (get-coord 3 'x coordinates) amount5) (+ (get-coord 3 'y coordinates) amount6));Trejde koordinaten
                              (list (+ (get-coord 4 'x coordinates) amount7) (+ (get-coord 4 'y coordinates) amount8)))));Fjärde koordinaten

                             ;; ändrade från cons till list då: (cadr (car '((4 . 2) (5 . 2) (6 . 2) (5 . 3)))) inte funkar med par (används i insert-block)
    
    (define/public (move-down)
      (move-coords 0 1 0 1 0 1 0 1))
    
;    (define/public (move-down)
;      (set! coordinates (list (cons (+ (get-coord 1 'x coordinates) 1) (get-coord 1 'y coordinates));Första koordinaten
;                              (cons (+ (get-coord 2 'x coordinates) 1) (get-coord 2 'y coordinates));Andra koordinaten
;                              (cons (+ (get-coord 3 'x coordinates) 1) (get-coord 3 'y coordinates));Trejde koordinaten
;                              (cons (+ (get-coord 4 'x coordinates) 1) (get-coord 4 'y coordinates)))));Fjärde koordinaten

    (define/public (move-direction direction)
      (cond
        ((eq? direction 'right)
         (move-coords 0 1 0 1 0 1 0 1))
        ((eq? direction 'left)
         (move-coords 0 -1 0 -1 0 -1 0 -1))))
    
;    (define/public (move-direction direction);mata in symbolerna 'right eller 'left för att flytta blocken åt det hållet.
;      (cond
;        ((eq? direction 'right)
;         (set! coordinates (list (cons (+ (get-coord 1 'x coordinates) 1) (get-coord 1 'y coordinates));Första koordinaten
;                                 (cons (+ (get-coord 2 'x coordinates) 1) (get-coord 2 'y coordinates));Andra koordinaten
;                                 (cons (+ (get-coord 3 'x coordinates) 1) (get-coord 3 'y coordinates));Trejde koordinaten
;                                 (cons (+ (get-coord 4 'x coordinates) 1) (get-coord 4 'y coordinates)))));Fjärde koordinaten
;        ((eq? direction 'left)
;         (set! coordinates (list (cons (- (get-coord 1 'x coordinates) 1) (get-coord 1 'y coordinates));Första koordinaten
;                                 (cons (- (get-coord 2 'x coordinates) 1) (get-coord 2 'y coordinates));Andra koordinaten
;                                 (cons (- (get-coord 3 'x coordinates) 1) (get-coord 3 'y coordinates));Trejde koordinaten
;                                 (cons (- (get-coord 4 'x coordinates) 1) (get-coord 4 'y coordinates)))))));Fjärde koordinaten
            

    (define/public (rotate direction);Säger hur koordinaterna ska ändras när ett visst block i en viss rotation roteras. ;ser hemskt ut....
      (cond
        ((eq? type 'I)
         (cond
           ((eq? rotation 'up)
            ((if (eq? direction 'right)
                 (move-coords 2 -2 1 -1 0 0 -1 1)
                 (move-coords 1 1 0 0 -1 -1 -2 -2))))
           ((eq? rotation 'right)
            ((if (eq? direction 'right)
                 (move-coords 1 1 0 0 -1 -1 -2 -2)
                 (move-coords -2 2 -1 1 0 0 1 -1))))
           ((eq? rotation 'down)
            ((if (eq? direction 'right)
                 (move-coords -2 2 -1 1 0 0 -1 1)
                 (move-coords -1 -1 0 0 1 1 2 2))))
           ((eq? rotation 'left)
            ((if (eq? direction 'right)
                 (move-coords -1 -1 0 0 1 1 2 2)
                 (move-coords 2 -2 1 -1 0 0 1 -1))))))
        ((eq? type 'J)
         (cond
           ((eq? rotation 'up)
            ((if (eq? direction 'right)
                 (move-coords 1 -1 0 0 -1 1 -2 0)
                 (move-coords 1 1 0 0 -1 -1 0 -2))))
           ((eq? rotation 'right)
            ((if (eq? direction 'right)
                 (move-coords 1 1 0 0 -1 -1 0 -2)
                 (move-coords -1 1 0 0 1 -1 2 0))))
           ((eq? rotation 'down)
            ((if (eq? direction 'right)
                 (move-coords -1 1 0 0 1 -1 2 0)
                 (move-coords -1 -1 0 0 1 1 0 2))))
           ((eq? rotation 'left)
            ((if (eq? direction 'right)
                 (move-coords -1 -1 0 0 1 1 0 2)
                 (move-coords 1 -1 0 0 -1 1 -2 0))))))
        ((eq? type 'L)
         (cond
           ((eq? rotation 'up)
            ((if (eq? direction 'right)
                 (move-coords -1 1 0 0 -1 1 0 2)
                 (move-coords -1 -1 0 0 1 1 2 0))))
           ((eq? rotation 'right)
            ((if (eq? direction 'right)
                 (move-coords -1 -1 0 0 1 1 2 0)
                 (move-coords 1 -1 0 0 1 -1 0 -2))))
           ((eq? rotation 'down)
            ((if (eq? direction 'right)
                 (move-coords 1 -1 0 0 -1 1 0 2)
                 (move-coords 1 1 0 0 -1 -1 -2 0))))
           ((eq? rotation 'left)
            ((if (eq? direction 'right)
                 (move-coords 1 1 0 0 -1 -1 -2 0)
                 (move-coords -1 1 0 0 1 -1 0 -2))))))
        ((eq? type 'O)
         (cond
           ((eq? rotation 'up)
            ((if (eq? direction 'right)
                 (move-coords -1 0 0 -1 1 0 0 1)
                 (move-coords 0 -1 1 0 0 1 -1 0))))
           ((eq? rotation 'right)
            ((if (eq? direction 'right)
                 (move-coords 0 -1 1 0 0 1 -1 0)
                 (move-coords 1 0 0 1 -1 0 0 -1))))
           ((eq? rotation 'down)
            ((if (eq? direction 'right)
                 (move-coords 1 0 0 1 -1 0 0 -1)
                 (move-coords 0 1 -1 0 0 -1 1 0))))
           ((eq? rotation 'left)
            ((if (eq? direction 'right)
                 (move-coords 0 1 -1 0 0 -1 1 0)
                 (move-coords -1 0 0 -1 1 0 0 1))))))
        ((eq? type 'S)
         (cond
           ((eq? rotation 'up)
            ((if (eq? direction 'right)
                 (move-coords 1 -1 0 0 1 1 0 2)
                 (move-coords 1 1 0 0 -1 1 -2 0))))
           ((eq? rotation 'right)
            ((if (eq? direction 'right)
                 (move-coords 1 1 0  0 -1 1 -2 0)
                 (move-coords -1 1 0 0 -1 -1 0 -2))))
           ((eq? rotation 'down)
            ((if (eq? direction 'right)
                 (move-coords -1 1 0 0 -1 -1 0 -2)
                 (move-coords -1 -1 0 0 1 -1 2 0))))
           ((eq? rotation 'left)
            ((if (eq? direction 'right)
                 (move-coords -1 -1 0 0 1 -1 2 0)
                 (move-coords -1 1 0 0 -1 -1 0 -2))))))
        ((eq? type 'T)
         (cond
           ((eq? rotation 'up)
            ((if (eq? direction 'right)
                 (move-coords 1 -1 1 1 -1 1 0 0)
                 (move-coords 1 1 -1 1 -1 -1 0 0))))
           ((eq? rotation 'right)
            ((if (eq? direction 'right)
                 (move-coords 1 1 -1 1 -1 -1 0 0)
                 (move-coords -1 1 -1 -1 1 -1 0 0))))
           ((eq? rotation 'down)
            ((if (eq? direction 'right)
                 (move-coords -1 1 -1 -1 1 -1 0 0)
                 (move-coords -1 -1 1 -1 1 1 0 0))))
           ((eq? rotation 'left)
            ((if (eq? direction 'right)
                 (move-coords -1 -1 1 -1 1 1 0 0)
                 (move-coords 1 -1 1 1 -1 1 0 0))))))
         ((eq? type 'Z)
          (cond
            ((eq? rotation 'up)
             ((if (eq? direction 'right)
                  (move-coords -1 1 0 0 1 1 2 0)
                  (move-coords -1 -1 0 0 -1 1 0 2))))
            ((eq? rotation 'right)
             ((if (eq? direction 'right)
                  (move-coords -1 -1 0 0 -1 1 0 2)
                  (move-coords 1 -1 0 0 -1 -1 -2 0)))
            ((eq? rotation 'down)
             ((if (eq? direction 'right)
                  (move-coords 1 -1 0 0 -1 -1 -2 0)
                  (move-coords 1 1 0 0 1 -1 0 -2))))
            ((eq? rotation 'left)
             ((if (eq? direction 'right)
                  (move-coords 1 1 0 0 1 -1 0 -2)
                  (move-coords -1 1 0 0 1 1 2 0)))))))))
    
        

    (define/public (get-place);Ska returnera koordinaterna som par i en lista
      coordinates)

    (super-new)))
    
    
    ;fall, move: direciton, rotate, create-new, get-place- get speed- get-rotation.