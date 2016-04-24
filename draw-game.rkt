#lang racket/gui
(require "block.rkt")
(require "board.rkt")
(require "game-init.rkt")
(require "cmd_store.rkt")

(define *window* (new frame%
                     [label "window"]
                     [width 900]
                     [height 600]
                     [x 0]	 
                     [y 0]))
(send *window* show #t)

(define (random-color)
  (let ((color-list '("blue" "red" "yellow" "orange" "lime" "magenta" "cyan")))
    (car (shuffle color-list))))

;; Ritar block givet lista av blockets koordinater (tex (send *I* get-place)). Inargument: canvas dc block
(define (draw-block canvas block-dc block color)
  (let ((part1 (first block))
        (part2 (second block))
        (part3 (third block))
        (part4 (fourth block)))
    (send block-dc set-brush color 'solid)
    (send block-dc draw-rectangle (car part1) (cadr part1) 20 20) ;; alla fyra delar bildar ett helt block
    (send block-dc draw-rectangle (car part2) (cadr part2) 20 20)
    (send block-dc draw-rectangle (car part3) (cadr part3) 20 20)
    (send block-dc draw-rectangle (car part4) (cadr part4) 20 20)))
  

(define (draw-grid canvas dc x y width height color)
  (send dc draw-rectangle x y width height) ;; x y width height
  (send dc set-brush color 'solid)
  (send dc set-pen "black" 2 'solid))

(define (draw-board canvas dc) ;; hämta board från game-init
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
     (draw-grid canvas dc x y 20 20 "white"))
    ((= (car items) 0)
     (begin (draw-grid canvas dc x y 20 20 "white")
            (draw-lines canvas dc (cdr items) (+ x 20) y)))
    ((= (car items) 1)
     (begin (draw-grid canvas dc x y 20 20 "green")
            (draw-lines canvas dc (cdr items) (+ x 20) y)))))

(define (draw-cycle canvas dc); Stoppa in alla saker som ska ritas i denna.
  (draw-block canvas dc (send *I* get-place) (random-color))
  (draw-board canvas dc))
 ; (send *I* move-down))...
 ; (send *a-canvas* refresh-now))

;; timers ska vara någon annan stans...
;(define *timer* (new timer%
;             [notify-callback draw-cycle]))

;(send *timer* start 1000 #f)

;;;;;;;;; Ska vi ha allt i samma canvas? (vet inte vad som blir bättre)
(define *a-canvas*
  (new canvas%
       [parent *window*]
       [paint-callback draw-cycle]))