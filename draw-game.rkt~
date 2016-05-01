#lang racket/gui
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
(send *window* show #t)

(define (draw-grid canvas dc x y width height color)
  (send dc draw-rectangle x y width height)
  (send dc set-brush color 'solid)
  (send dc set-pen "black" 2 'solid))

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
            (begin (draw-grid canvas dc x y 20 20 "white")))
           ((= (car items) 1)
            (begin (draw-grid canvas dc x y 20 20 "lime"))))) ;; tillfällig lösning
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


;; Allt som ska ritas stoppas här.
(define (draw-cycle canvas dc)
  (let ((cur-block (send *board-1* get-cur-block)))
        ;(cur-block-color (send (send *board-1* get-cur-block) get-color) ; men siffra i nuläget
  (draw-board canvas dc)
  (draw-block canvas dc (send cur-block get-place) "cyan")
  (draw-block canvas dc (send *I* get-place) "magenta")))
  ;(send *a-canvas* refresh-now))

(define (refresh-draw-cycle)
  (send *a-canvas* refresh-now))

(define *draw-timer* (new timer%
                     [notify-callback refresh-draw-cycle]))
(send *draw-timer* start 60 #f)

;; ska anropa fall senare. Nu rör den sig ner i oändligheten.
(define (draw-fall)
  (let ((cur-block (send *board-1* get-cur-block)))
   ; (send cur-block fall)  ; current-block
  (send *I* move-down)
  ))

(define *fall-timer* (new timer%
                     [notify-callback draw-fall]))
(send *fall-timer* start 1000 #f)

(define *a-canvas*
  (new canvas%
       [parent *window*]
       [paint-callback draw-cycle]))

(provide (all-defined-out))