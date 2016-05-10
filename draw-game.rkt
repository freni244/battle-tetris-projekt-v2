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

(define (draw-board canvas dc board x y) ;; hämtar och ritar board från game-init
  (draw-board-help canvas dc x y (send board get-matrix)))

(define (draw-board-help canvas dc x y items)
  (cond
    ((empty? (cdr items))
     (draw-lines canvas dc (car items) x y))
    (else (begin (draw-lines canvas dc (car items) x y)
                 (draw-board-help canvas dc x (+ 20 y) (cdr items))))))

(define (draw-lines canvas dc items x y)
  (let ((color (return-color-from-num (car items))))
    (cond
      ((empty? (cdr items))
       (draw-grid canvas dc x y 20 20 color))
      (else
       (begin (draw-grid canvas dc x y 20 20 color)
              (draw-lines canvas dc (cdr items) (+ x 20) y))))))

;(define (random-color)
;  (let ((color-list '("blue" "red" "yellow" "orange" "lime" "magenta" "cyan")))
;    (car (shuffle color-list))))

;; Ritar block givet lista av blockets koordinater (tex (send *I* get-place)).
(define (draw-block canvas dc block color x y)
  (let ((part1 (first block))
        (part2 (second block))
        (part3 (third block))
        (part4 (fourth block)))
    (send dc set-brush color 'solid)
    (send dc draw-rectangle (+ x (* (- (car part1) 1) 20)) (+ y (* (- (cadr part1) 1) 20)) 20 20) ;; alla fyra delar bildar ett helt block
    (send dc draw-rectangle (+ x (* (- (car part2) 1) 20)) (+ y (* (- (cadr part2) 1) 20)) 20 20) ;; koordinaterna multipliceras med 20 så att det motsvarar
    (send dc draw-rectangle (+ x (* (- (car part3) 1) 20)) (+ y (* (- (cadr part3) 1) 20)) 20 20) ;; ett steg på 20 pixlar
    (send dc draw-rectangle (+ x (* (- (car part4) 1) 20)) (+ y (* (- (cadr part4) 1) 20)) 20 20)))

;; tillfälligt... 
(define (draw-text canvas dc)
  (send dc draw-text "You lose!" 180 75))

;; Allt som ska ritas stoppas här.
(define (draw-cycle canvas dc)
  (let ((cur-block-b1 (send *board-1* get-cur-block)) ;; "current block board-1"
        (block-color-b1 (send (send *board-1* get-cur-block) get-color-name)) ;; färgen hos cur-block-b1
        (cur-block-b2 (send *board-2* get-cur-block))
        (block-color-b2 (send (send *board-2* get-cur-block) get-color-name)))
    (draw-board canvas dc *board-1* 500 100)
    (draw-board canvas dc *board-2* 100 100)
    (draw-block canvas dc (send cur-block-b1 get-place) block-color-b1 500 100)
    (draw-block canvas dc (send cur-block-b2 get-place) block-color-b2 100 100)
    ))
  ;(send *board-1-canvas* refresh-now))

(define (refresh-draw-cycle)
  (send *game-canvas* refresh-now))

(define (generate-block board)
  (let ((blocks (send board get-all-types)))
    (car (shuffle blocks))))

;;;;;;;;; board istället för *board-1*...
(define (draw-fall board)
  (let ((cur-block (send board get-cur-block))
        (block-color (send (send board get-cur-block) get-color-num))
        (block-coord (send (send board get-cur-block) get-place))
        (next-block-coord (send (send board get-cur-block) return-move-down))
        (occupied-coord (send board get-occupied-coord))
        (bottom (send board get-bottom))
        (new-block (generate-block board)))
    (cond ((or (occurs-coordinates? next-block-coord occupied-coord) (occurs-coordinates? block-coord bottom))
           (send board remove-cur-block)
           (send cur-block reset-coord)
           (send board insert-block block-color block-coord) ;; sätter in blocket i board.
           (send board queue-block new-block)) ;; lägger ett block på kö
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

(define *game-canvas*
  (new input-canvas%
       [parent *window*]
       [paint-callback draw-cycle]
       [keyboard-handler (lambda (key-event)
                           (let ((key-code (send key-event get-key-code)))
                             (if (not (equal? key-code 'release))
                                 (begin (move key-code *board-1*)
                                        (move key-code *board-2*))
                                 (void))))]))

(send *game-canvas* focus)

(provide (all-defined-out))