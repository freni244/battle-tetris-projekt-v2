#lang racket
(provide block%)

(define (get-coord n xy items);Hämtar x/y koordinaten på n:e plats i listan.
  (if (= n 1)
      (if (eq? xy 'x)
          (caar items)
          (cadar items))
      (get-coord (- n 1) xy (cdr items))))
      

(define block%
  (class object%
    (init-field coordinates
                type)
    (field [place '()]
           [hold '()]
           [rotation 'up])

    (define/public (fall);Ska skicka blocket ned i botten.
      1)

    (define/public (move-down)
      (set! coordinates (list (cons (+ (get-coord 1 'x coordinates) 1) (get-coord 1 'y coordinates));Första koordinaten
                              (cons (+ (get-coord 2 'x coordinates) 1) (get-coord 2 'y coordinates));Andra koordinaten
                              (cons (+ (get-coord 3 'x coordinates) 1) (get-coord 3 'y coordinates));Trejde koordinaten
                              (cons (+ (get-coord 4 'x coordinates) 1) (get-coord 4 'y coordinates)))));Fjärde koordinaten
    
    (define/public (move-direction)
      (cond
        ((eq? direction 'right)
         (set! coordinates (list (cons (+ (get-coord 1 'x coordinates) 1) (get-coord 1 'y coordinates));Första koordinaten
                                 (cons (+ (get-coord 2 'x coordinates) 1) (get-coord 2 'y coordinates));Andra koordinaten
                                 (cons (+ (get-coord 3 'x coordinates) 1) (get-coord 3 'y coordinates));Trejde koordinaten
                                 (cons (+ (get-coord 4 'x coordinates) 1) (get-coord 4 'y coordinates)))));Fjärde koordinaten
        ((eq? direction 'left)
         (set! coordinates (list (cons (- (get-coord 1 'x coordinates) 1) (get-coord 1 'y coordinates));Första koordinaten
                                 (cons (- (get-coord 2 'x coordinates) 1) (get-coord 2 'y coordinates));Andra koordinaten
                                 (cons (- (get-coord 3 'x coordinates) 1) (get-coord 3 'y coordinates));Trejde koordinaten
                                 (cons (- (get-coord 4 'x coordinates) 1) (get-coord 4 'y coordinates)))))));Fjärde koordinaten
            

    (define/public (rotate)
      1)

    (define/public (create-new)
      1)

    (define/public (get-place);Ska returnera koordinaterna som par i en lista
      coordinates)

    (super-new)))
    
    
    ;fall, move: direciton, rotate, create-new, get-place- get speed- get-rotation.