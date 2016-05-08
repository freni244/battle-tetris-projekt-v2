#lang racket/gui
;(provide *draw-timer*)
;(provide *fall-timer*)
(require "block.rkt")
(require "board.rkt")
(require "game-init.rkt")
(require "movement-and-cmd.rkt")

(define *window* (new frame%
                     [label "window"]
                     [width 900]
                     [height 600]
                     [x 0]	 
                     [y 0]))
;(send *window* show #t)

(define (draw-grid canvas dc x y width height color)
  (send dc set-brush color 'solid)
  (send dc set-pen "black" 2 'solid)
  (send dc draw-rectangle x y width height))

(define (draw-board canvas dc) ;; hämtar och ritar board från game-init
  (draw-board-help canvas dc 100 100 (send *board-1* get-matrix)))

(define (draw-board-help canvas dc x y items)
  (cond
    ((empty? (cdr items))
     (draw-lines canvas dc (car items) x y))
    (else (begin (draw-lines canvas dc (car items) x y)
                 (draw-board-help canvas dc x (+ 20 y) (cdr items))))))

(define (draw-lines canvas dc items x y)
  (cond
    ((empty? (cdr items))
     (cond ((= (car items) 0)
            (draw-grid canvas dc x y 20 20 "white"))
           ((= (car items) 1)
            (draw-grid canvas dc x y 20 20 "lime"))))    ;; tillfällig lösning
    ((= (car items) 0)
     (begin (draw-grid canvas dc x y 20 20 "white")
            (draw-lines canvas dc (cdr items) (+ x 20) y)))
    ((= (car items) 1)
     (begin (draw-grid canvas dc x y 20 20 "lime")
            (draw-lines canvas dc (cdr items) (+ x 20) y)))
    ((= (car items) 2)
     (begin (draw-grid canvas dc x y 20 20 "blue")
            (draw-lines canvas dc (cdr items) (+ x 20) y)))
    ((= (car items) 3)
     (begin (draw-grid canvas dc x y 20 20 "red")
            (draw-lines canvas dc (cdr items) (+ x 20) y)))
    ((= (car items) 4)
     (begin (draw-grid canvas dc x y 20 20 "yellow")
            (draw-lines canvas dc (cdr items) (+ x 20) y)))
    ((= (car items) 5)
     (begin (draw-grid canvas dc x y 20 20 "orange")
            (draw-lines canvas dc (cdr items) (+ x 20) y)))
    ((= (car items) 6)
     (begin (draw-grid canvas dc x y 20 20 "cyan")
            (draw-lines canvas dc (cdr items) (+ x 20) y)))
    ((= (car items) 7)
     (begin (draw-grid canvas dc x y 20 20 "magenta")
            (draw-lines canvas dc (cdr items) (+ x 20) y)))))

(define (random-color)
  (let ((color-list '("blue" "red" "yellow" "orange" "lime" "magenta" "cyan")))
    (car (shuffle color-list))))

;; Ritar block givet lista av blockets koordinater (tex (send *I* get-place)).
(define (draw-block canvas dc block color)
  (let ((part1 (first block))
        (part2 (second block))
        (part3 (third block))
        (part4 (fourth block)))
    (send dc set-brush color 'solid)
    (send dc draw-rectangle (+ 100 (* (- (car part1) 1) 20)) (+ 100 (* (- (cadr part1) 1) 20)) 20 20) ;; alla fyra delar bildar ett helt block
    (send dc draw-rectangle (+ 100 (* (- (car part2) 1) 20)) (+ 100 (* (- (cadr part2) 1) 20)) 20 20) ;; koordinaterna multipliceras med 20 så att det motsvarar
    (send dc draw-rectangle (+ 100 (* (- (car part3) 1) 20)) (+ 100 (* (- (cadr part3) 1) 20)) 20 20) ;; ett steg på 20 pixlar
    (send dc draw-rectangle (+ 100 (* (- (car part4) 1) 20)) (+ 100 (* (- (cadr part4) 1) 20)) 20 20)))

;; tillfälligt... 
(define (draw-text canvas dc)
  (send dc draw-text "You lose!" 180 75))

;; Allt som ska ritas stoppas här.
(define (draw-cycle canvas dc)
  (let ((cur-block (send *board-1* get-cur-block))
        (block-color (send (send *board-1* get-cur-block) get-color-name)))
    (draw-board canvas dc)
    (draw-block canvas dc (send cur-block get-place) block-color)
    ;(draw-block canvas dc (send *I* get-place) "magenta")
    ))
  ;(send *board-1-canvas* refresh-now))v

(define (refresh-draw-cycle)
  (send *board-1-canvas* refresh-now))

(define (generate-block)
  (let ((blocks (send *board-1* get-all-types)))
    (car (shuffle blocks))))

;; ska anropa fall senare. Nu rör den sig ner i oändligheten.
(define (draw-fall)
  (let ((cur-block (send *board-1* get-cur-block))
        (block-color (send (send *board-1* get-cur-block) get-color-num))
        (block-coord (send (send *board-1* get-cur-block) get-place))
        (next-block-coord (send (send *board-1* get-cur-block) return-move-down))
        (occupied-coord (send *board-1* get-occupied-coord))
        (bottom (send *board-1* get-bottom))
        (new-block (generate-block)))
    (cond ((or (occurs-coordinates? next-block-coord occupied-coord) (occurs-coordinates? block-coord bottom))
           (send *board-1* remove-cur-block)
           (send cur-block reset-coord)
           (send *board-1* insert-block block-color block-coord) ;; sätter in blocket i board.
           (send *board-1* queue-block new-block)) ;; lägger ett block på kö
          (else (send cur-block move-down)))))

;(define *draw-timer* (new timer%
;                     [notify-callback refresh-draw-cycle]))
;(send *draw-timer* start 16 #f) ;; aktiveras i main...
;
;(define *fall-timer* (new timer%
;                     [notify-callback draw-fall]))
;(send *fall-timer* start 300 #f)

;; Subklass av canvas%, som kan hantera key-events
(define input-canvas%
  (class canvas%
       (init-field keyboard-handler)
       (define/override (on-char key-event)
         (keyboard-handler key-event))
       (super-new)))

(define *board-1-canvas* ;; vet inte om det är till en fördel att dela upp det i *board-1-canvas*/*board-2-canvas*
  (new input-canvas%
       [parent *window*]
       [paint-callback draw-cycle]
       [keyboard-handler (lambda (key-event)
                           (let ((key-code (send key-event get-key-code)))
                             (if (not (equal? key-code 'release))
                                 (move key-code)  ;(send *my-rotating-image* key-down key-code)
                                 (void))))]))

(send *board-1-canvas* focus)

(provide (all-defined-out))