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

(define (draw-grid canvas dc x y width height color)
  (send dc set-brush color 'solid)
  (send dc set-pen "black" 2 'solid)
  (send dc draw-rectangle x y width height))

;; Ritar spelbrädet. Inargument: board (spelbräde som objekt), x, y (spelbrädets koordinater), canvas, dc.
(define (draw-board canvas dc board x y) ;; hämtar och ritar board från game-init
  (draw-board-help canvas dc x y (send board get-matrix)))

;; Hjälpfunktion till draw-board. Items är en matris av listor.
(define (draw-board-help canvas dc x y items)
  (cond
    ((empty? (cdr items))
     (draw-lines canvas dc (car items) x y))
    (else (begin (draw-lines canvas dc (car items) x y)
                 (draw-board-help canvas dc x (+ 20 y) (cdr items))))))

;; Ritar rader. Items är en lista med nummer. Ett nummer motsvarar en färg.
(define (draw-lines canvas dc items x y)
  (let ((color (return-color-from-num (car items))))
    (cond
      ((empty? (cdr items))
       (draw-grid canvas dc x y 20 20 color))
      (else
       (begin (draw-grid canvas dc x y 20 20 color)
              (draw-lines canvas dc (cdr items) (+ x 20) y))))))

;; Ritar block givet lista av blockets koordinater. Inargument: canvas, dc, block, color (nummer), x, y.
(define (draw-block canvas dc block color x y)
    (send dc set-brush color 'solid)
    (send dc draw-rectangle (+ x (* (- (send block get-x-part1) 1) 20)) (+ y (* (- (send block get-y-part1) 1) 20)) 20 20) ;; alla fyra delar bildar ett helt block
    (send dc draw-rectangle (+ x (* (- (send block get-x-part2) 1) 20)) (+ y (* (- (send block get-y-part2) 1) 20)) 20 20) ;; koordinaterna multipliceras med 20 så att det motsvarar
    (send dc draw-rectangle (+ x (* (- (send block get-x-part3) 1) 20)) (+ y (* (- (send block get-y-part3) 1) 20)) 20 20) ;; ett steg på 20 pixlar
    (send dc draw-rectangle (+ x (* (- (send block get-x-part4) 1) 20)) (+ y (* (- (send block get-y-part4) 1) 20)) 20 20))

;;  
(define (draw-text canvas dc)
  (cond ((not (send *board-1* in-game?))  ;; om spelplan-1 inte är "in-game"
         (send dc set-font (make-font #:size 40 #:family 'roman
                                      #:weight 'bold))
         (send dc set-text-foreground "red")
         (send dc draw-text "You lose!" 520 50)
         (send dc set-text-foreground "blue")
         (send dc draw-text "You won!" 120 50)
         )
        ((not (send *board-2* in-game?))  ;; om spelplan-1 inte är "in-game"
         (send dc set-font (make-font #:size 40 #:family 'roman
                                      #:weight 'bold))
         (send dc set-text-foreground "red")
         (send dc draw-text "You lose!" 120 50)
         (send dc set-text-foreground "blue")
         (send dc draw-text "You won!" 520 50)
         )
        (else void)))

(define (draw-score canvas dc b1-score b2-score)
  (let ((b1-score-text (string-join (append '("Score: ") (list b1-score))))
        (b2-score-text (string-join (append '("Score: ") (list b2-score)))))
    (send dc set-font (make-font #:size 20 #:family 'roman
                                 #:weight 'bold))
    (send dc set-text-foreground "black")
    (send dc draw-text b1-score-text 500 500)
    (send dc draw-text b2-score-text 100 500)))

;; Allt som ska ritas stoppas här.
(define (draw-cycle canvas dc)
  (let ((cur-block-b1 (send *board-1* get-cur-block)) ;; "current block board-1"
        (block-color-b1 (send (send *board-1* get-cur-block) get-color-name)) ;; färgen hos cur-block-b1
        (cur-block-b2 (send *board-2* get-cur-block))
        (block-color-b2 (send (send *board-2* get-cur-block) get-color-name)))
    (draw-board canvas dc *board-1* 500 100)
    (draw-board canvas dc *board-2* 100 100)
    (draw-block canvas dc cur-block-b1 block-color-b1 500 100)       ;(draw-block canvas dc (send cur-block-b1 get-place) block-color-b1 500 100)
    (draw-block canvas dc cur-block-b2 block-color-b2 100 100)
    (draw-text canvas dc)
    (draw-score canvas dc (number->string (send *board-1* get-score)) (number->string (send *board-2* get-score)))
    ))

(define (refresh-draw-cycle)
  (send *game-canvas* refresh-now))

;; Genererar ett random block utav alla typer som finns i board.
(define (generate-block board)
  (let ((blocks (send board get-all-types)))
    (car (shuffle blocks))))

;; Får block att falla i givet spelbräde.
(define (draw-fall board)
  (let ((cur-block (send board get-cur-block))
        (block-color (send (send board get-cur-block) get-color-num))
        (block-coord (send (send board get-cur-block) get-place))
        (next-block-coord (send (send board get-cur-block) return-move-down))
        (occupied-coord (send board get-occupied-coord))
        (bottom (send board get-bottom))
        (new-block (generate-block board)))
    (cond ((or (occurs-coordinates? next-block-coord occupied-coord) (occurs-coordinates? block-coord bottom))
           (send board insert-block block-color cur-block) ;; sätter in blocket i board.
           (send cur-block reset-block)
           (send board remove-cur-block)
           (send board queue-block new-block) ;; lägger ett block på kö
           (send board add-point 1))  ;; ge ett poäng
          (else (send cur-block move-down)))))

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
;;;; På "key down" lägg till tangenter i lista. Key up ta bort. Fortsätt med key-event så länge key ligger i listan. (inte använda equal?... "key=?"?)
;;;; on-key,  on-release
                                 
(send *game-canvas* focus) ;; gör att tangentbordshändelser har "fokus" på *game-canvas*

(provide (all-defined-out))