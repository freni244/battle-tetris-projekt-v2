#lang racket
(provide block%)

(define block%
  (class object%
    (init-field coordinates
                start-coordinates
                type
                color) ;; färgen är en siffra mellan 1-7
    (field [place '()]
           [hold '()]
           [rotation 'up]
           [start-rotation 'up])

    (define/public (get-color-num) color) ;; returns color as number

    (define/public (get-color-name) ;; returns name of color
      (cond ((= color 1) "lime")
            ((= color 2) "blue")
            ((= color 3) "red")
            ((= color 4) "yellow")
            ((= color 5) "orange")
            ((= color 6) "cyan")
            ((= color 7) "magenta")
            (else "gold")))      
    
    ;(define/public (generate-block)...

    (define/public (occurs? el list)
      (not (null? (filter (lambda (x) (equal? x el)) list))))

    (define/public (get-coord n xy items);Hämtar x/y koordinaten på n:e plats i listan.
      (if (= n 1)
          (if (eq? xy 'x)
              (caar items)
              (cadar items))
          (get-coord (- n 1) xy (cdr items))))
    
    ;; adderar amount till koordinaten, kan vara negativ. (returnerar endast, ej set!)
    (define/public (move-coords amount1 amount2 amount3 amount4 amount5 amount6 amount7 amount8);ändrar koordinaterna mha proc och amount,proc + eller -, amount hur mycket den ska flyttas.
      (list (list (+ (get-coord 1 'x coordinates) amount1) (+ (get-coord 1 'y coordinates) amount2));Första koordinaten
            (list (+ (get-coord 2 'x coordinates) amount3) (+ (get-coord 2 'y coordinates) amount4));Andra koordinaten
            (list (+ (get-coord 3 'x coordinates) amount5) (+ (get-coord 3 'y coordinates) amount6));Trejde koordinaten
            (list (+ (get-coord 4 'x coordinates) amount7) (+ (get-coord 4 'y coordinates) amount8))));Fjärde koordinaten

    ;; Returnerar koordinaterna "ett steg ner".
    (define/public (return-move-down)
      (move-coords 0 1 0 1 0 1 0 1))

    ;; Flyttar block ett steg ner. (med set!)
    (define/public (move-down)
      (set! coordinates (return-move-down)))

    ;; Returnerar koordinater ett steg höger eller vänster.
    (define/public (return-move-direction direction)
      (cond
        ((eq? direction 'right)
         (move-coords 1 0 1 0 1 0 1 0))
        ((eq? direction 'left)
         (move-coords -1 0 -1 0 -1 0 -1 0))))

    ;; Flyttar blocket vänster eller höger. (med set!)
    (define/public (move-direction direction)
      (set! coordinates (return-move-direction direction)))
                
    (define/public (get-rotation)
      rotation)

    ;; Ändrar till roterade koordinater
    (define/public (rotate direction)
      (set! coordinates (return-rotate direction))
      (cond ((eq? rotation 'up)
             (if (eq? direction 'right)
                 (set! rotation 'right)
                 (set! rotation 'left)))
            ((eq? rotation 'right)
             (if (eq? direction 'right)
                 (set! rotation 'down)
                 (set! rotation 'up)))
            ((eq? rotation 'down)
             (if (eq? direction 'right)
                 (set! rotation 'left)
                 (set! rotation 'right)))
            ((eq? rotation 'left)
             (if (eq? direction 'right)
                 (set! rotation 'up)
                 (set! rotation 'down)))))

    ;; returnerar roterade koordinater (även set!, men det -> fel då meningen med return-rotate är att även kunna kolla om det går att rotera utan att ändra)
    (define/public (return-rotate direction);Säger hur koordinaterna ska ändras när ett visst block i en viss rotation roteras. ;ser hemskt ut....
      (cond
        ((eq? type 'I)
         (cond
           ((eq? rotation 'up)
            (cond ((eq? direction 'right)
                   (move-coords 2 -2 1 -1 0 0 -1 1))
                  (else
                   (move-coords 1 1 0 0 -1 -1 -2 -2))))
           ((eq? rotation 'right)
            (cond ((eq? direction 'right)
                   (move-coords 1 1 0 0 -1 -1 -2 -2))
                  (else
                   (move-coords -2 2 -1 1 0 0 1 -1))))
           ((eq? rotation 'down)
            (cond ((eq? direction 'right)
                   (move-coords -2 2 -1 1 0 0 1 -1))
                  (else
                   (move-coords -1 -1 0 0 1 1 2 2))))
           ((eq? rotation 'left)
            (cond ((eq? direction 'right)
                   (move-coords -1 -1 0 0 1 1 2 2))
                  (else
                   (move-coords 2 -2 1 -1 0 0 -1 1))))
           (else #f)))

        ((eq? type 'J)
         (cond
           ((eq? rotation 'up)
            (cond ((eq? direction 'right)
                   (move-coords 1 -1 0 0 -1 1 -2 0))
                  (else
                   (move-coords 1 1 0 0 -1 -1 0 -2))))
           ((eq? rotation 'right)
            (cond ((eq? direction 'right)
                   (move-coords 1 1 0 0 -1 -1 0 -2))
                  (else
                   (move-coords -1 1 0 0 1 -1 2 0))))
           ((eq? rotation 'down)
            (cond ((eq? direction 'right)
                   (move-coords -1 1 0 0 1 -1 2 0))
                  (else
                   (move-coords -1 -1 0 0 1 1 0 2))))
           ((eq? rotation 'left)
            (cond ((eq? direction 'right)
                   (move-coords -1 -1 0 0 1 1 0 2))
                  (else
                   (move-coords 1 -1 0 0 -1 1 -2 0))))
           (else #f)))
        ((eq? type 'L)
         (cond
           ((eq? rotation 'up)
            (cond ((eq? direction 'right)
                   (move-coords 1 -1 0 0 -1 1 0 -2)) 
                  (else
                   (move-coords 1 1 0 0 -1 -1 2 0))))
           ((eq? rotation 'right)
            (cond ((eq? direction 'right)
                   (move-coords 1 1 0 0 -1 -1 2 0))
                  (else
                   (move-coords -1 1 0 0 1 -1 0 2))))
           ((eq? rotation 'down)
            (cond ((eq? direction 'right)
                   (move-coords -1 1 0 0 1 -1 0 2))
                  (else
                   (move-coords -1 -1 0 0 1 1 -2 0))))
           ((eq? rotation 'left)
            (cond ((eq? direction 'right)
                   (move-coords -1 -1 0 0 1 1 -2 0))
                  (else
                   (move-coords 1 -1 0 0 -1 1 0 -2))))
           (else #f)))
        ((eq? type 'O)
         (cond
           ((eq? rotation 'up)
            (cond ((eq? direction 'right)
                   (move-coords 1 0 0 1 0 -1 -1 0))
                  (else
                   (move-coords 0 1 -1 0 1 0 0 -1))))
           ((eq? rotation 'right)
            (cond ((eq? direction 'right)
                   (move-coords 0 1 -1 0 1 0 0 -1))
                  (else
                   (move-coords -1 0 0 -1 0 1 1 0))))
           ((eq? rotation 'down)
            (cond ((eq? direction 'right)
                   (move-coords -1 0 0 -1 0 1 1 0))
                  (else
                   (move-coords  0 -1 1 0 -1 0 0 1))))
           ((eq? rotation 'left)
            (cond ((eq? direction 'right)
                   (move-coords 0 -1 1 0 -1 0 0 1))
                  (else
                   (move-coords 1 0 0 1 0 -1 -1 0))))
           (else #f)))
        ((eq? type 'S)
         (cond
           ((eq? rotation 'up)
            (cond ((eq? direction 'right)
                   (move-coords 0 0 -1 1 0 -2 -1 -1))
                  (else
                   (move-coords 0 0 -1 -1 2 0 1 -1))))
           ((eq? rotation 'right)
            (cond ((eq? direction 'right)
                   (move-coords 0 0 -1 -1 2 0 1 -1))
                  (else
                   (move-coords 0 0  1 -1 0 2 1 1))))
           ((eq? rotation 'down)
            (cond ((eq? direction 'right)
                   (move-coords 0 0  1 -1 0 2 1 1))
                  (else
                   (move-coords 0 0 1 1 -2 0 -1 1))))
           ((eq? rotation 'left)
            (cond ((eq? direction 'right)
                   (move-coords 0 0 1 1 -2 0 -1 1))
                  (else
                   (move-coords 0 0 -1 1 0 -2 -1 -1))))
           (else #f)))
        ((eq? type 'T)
         (cond
           ((eq? rotation 'up)
            (cond ((eq? direction 'right)
                   (move-coords 1 -1 0 0 -1 1 -1 -1))
                  (else
                   (move-coords 1 1 0 0 -1 -1 1 -1))))
           ((eq? rotation 'right)
            (cond ((eq? direction 'right)
                   (move-coords 1 1 0 0 -1 -1 1 -1))
                  (else
                   (move-coords -1 1 0 0 1 -1 1 1))))
           ((eq? rotation 'down)
            (cond ((eq? direction 'right)
                   (move-coords -1 1 0 0 1 -1 1 1))
                  (else
                   (move-coords -1 -1 0 0 1 1 -1 1))))
           ((eq? rotation 'left)
            (cond ((eq? direction 'right)
                   (move-coords -1 -1 0 0 1 1 -1 1))
                  (else
                   (move-coords 1 -1 0 0 -1 1 -1 -1))))
           (else #f)))
         ((eq? type 'Z)
          (cond
            ((eq? rotation 'up)
             (cond ((eq? direction 'right)
                    (move-coords 1 -1 0 0 -1 -1 -2 0))
                   (else
                    (move-coords 1 1 0 0 1 -1 0 -2))))
            ((eq? rotation 'right)
             (cond ((eq? direction 'right)
                    (move-coords 1 1 0 0 1 -1 0 -2))
                   (else
                    (move-coords -1 1 0 0 1 1 2 0))))
            ((eq? rotation 'down)
             (cond ((eq? direction 'right)
                    (move-coords -1 1 0 0 1 1 2 0))
                   (else
                    (move-coords -1 -1 0 0 -1 1 0 2))))
            ((eq? rotation 'left)
             (cond ((eq? direction 'right)
                    (move-coords -1 -1 0 0 -1 1 0 2))
                   (else
                    (move-coords 1 -1 0 0 -1 -1 -2 0))))
            (else #f)))
         (else
          (display "There is no such type "))))
    
    (define/public (get-place) ;; Returnerar koordinaterna som par i en lista
      coordinates)
    
    (define/public (reset-coord)
      (set! coordinates start-coordinates))

    (define/public (reset-rotation)
      (set! rotation start-rotation))
    
    (define/public (reset-block)
       (begin (reset-coord) (reset-rotation)))
    
    (super-new)))