#lang racket/gui
(require "block.rkt")
(require "board.rkt")
(require "game-init.rkt")
(require "movement-and-cmd.rkt")
(require racket/trace)

(define *window* (new frame%
                     [label "Battle Tetris"]
                     [width 900]
                     [height 600]
                     [x 0]	 
                     [y 0]))

(define *game-area*
  (new horizontal-panel%
       [parent *window*]
       [alignment '(center center)]
       [min-height 570]))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Spelbräde och block
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ritar spelbrädet. Inargument: board (spelbräde som objekt), x, y (spelbrädets koordinater), canvas, dc.
(define (draw-board canvas dc board x y)
  (define (draw-board-help canvas dc x y items) ;; Hjälpfunktion till draw-board. Items är en matris av listor.
    (if (empty? (cdr items))
        (draw-row canvas dc (car items) x y)
        (begin (draw-row canvas dc (car items) x y)
               (draw-board-help canvas dc x (+ 20 y) (cdr items)))))
  (draw-board-help canvas dc x y (send board get-matrix)))

;; Ritar en ruta med ifyllda kanter.
(define (draw-square canvas dc x y width height color)
  (send dc set-brush color 'solid)
  (send dc set-pen "black" 2 'solid)
  (send dc draw-rectangle x y width height))

;; Ritar rader. Items är en lista med nummer. Ett nummer motsvarar en färg.
(define (draw-row canvas dc items x y)
  (let ((color (return-color-from-num (car items))))
    (if (empty? (cdr items))
        (draw-square canvas dc x y 20 20 color)
        (begin (draw-square canvas dc x y 20 20 color)
               (draw-row canvas dc (cdr items) (+ x 20) y)))))

;; Ritar block givet lista av blockets koordinater. Inargument: canvas, dc, block, color (nummer), x, y.
(define (draw-block canvas dc block color x y width height)
    (send dc set-brush color 'solid)
    (send dc draw-rectangle (+ x (* (- (send block get-x-part1) 1) width)) (+ y (* (- (send block get-y-part1) 1) height)) width height) ;; alla fyra delar bildar ett helt block
    (send dc draw-rectangle (+ x (* (- (send block get-x-part2) 1) width)) (+ y (* (- (send block get-y-part2) 1) height)) width height) ;; koordinaterna multipliceras med 20 så att det motsvarar
    (send dc draw-rectangle (+ x (* (- (send block get-x-part3) 1) width)) (+ y (* (- (send block get-y-part3) 1) height)) width height) ;; ett steg på 20 pixlar
    (send dc draw-rectangle (+ x (* (- (send block get-x-part4) 1) width)) (+ y (* (- (send block get-y-part4) 1) height)) width height))

;; Ritar block givet lista av blockets koordinater. Inargument: canvas, dc, block, color (nummer), x, y.
;;;;;; Ful lösning för att visa nästa block utan att den ska ramla (repeterad kod). Ignorera att detta står med.
(define (draw-next-block canvas dc block color x y width height)
  (send dc set-brush color 'solid)
  (send dc draw-rectangle (+ x (* (- (send block get-x-part1-next-block) 1) width)) (+ y (* (- (send block get-y-part1-next-block) 1) height)) width height) ;; alla fyra delar bildar ett helt block
  (send dc draw-rectangle (+ x (* (- (send block get-x-part2-next-block) 1) width)) (+ y (* (- (send block get-y-part2-next-block) 1) height)) width height) ;; koordinaterna multipliceras med 20 så att det motsvarar
  (send dc draw-rectangle (+ x (* (- (send block get-x-part3-next-block) 1) width)) (+ y (* (- (send block get-y-part3-next-block) 1) height)) width height) ;; ett steg på 20 pixlar
  (send dc draw-rectangle (+ x (* (- (send block get-x-part4-next-block) 1) width)) (+ y (* (- (send block get-y-part4-next-block) 1) height)) width height))

;; Allt som ska ritas när man spelar på board-1. (spelbräde, block, score)
(define (show-board-1 canvas dc)
  (let ((cur-block-b1 (send *board-1* get-cur-block)) ;; "current block board-1"
        (block-color-b1 (send (send *board-1* get-cur-block) get-color-name)) ;; färgen hos cur-block-b1
        (next-block-b1 (send *board-1* get-next-block)) ;; ignorera 
        (next-block-color-b1 (send (send *board-1* get-next-block) get-color-name))) ;; ignorera
    (draw-board canvas dc *board-1* 500 100)
    (draw-block canvas dc cur-block-b1 block-color-b1 500 100 20 20)
    (draw-score canvas dc (number->string (send *board-1* get-score)) 500 500 "Score: ")
    (draw-next-block canvas dc next-block-b1 next-block-color-b1 675 100 10 10))) ;; ignorera att detta står med

;; Allt som ska ritas när man spelar på board-2. (spelbräde, block, score)
(define (show-board-2 canvas dc)
  (let ((cur-block-b2 (send *board-2* get-cur-block)) ;; "current block board-2"
        (block-color-b2 (send (send *board-2* get-cur-block) get-color-name)) ;; färgen hos cur-block-b1
        (next-block-b2 (send *board-2* get-next-block)) ;; ignorera 
        (next-block-color-b2 (send (send *board-2* get-next-block) get-color-name))) ;; ignorera
    (draw-board canvas dc *board-2* 100 100)
    (draw-block canvas dc cur-block-b2 block-color-b2 100 100 20 20)
    (draw-score canvas dc (number->string (send *board-2* get-score)) 100 500 "Score: ")
    (draw-score canvas dc (number->string (send *board-1* get-score)) 500 500 "Score: ")
    (draw-next-block canvas dc next-block-b2 next-block-color-b2 275 100 10 10))) ;; ignorera att detta står med

;;;;;;;;;;;;;;;;;;;;
;;;;;; Text
;;;;;;;;;;;;;;;;;;;;

(define (draw-game-title canvas dc)
  (send dc set-font (make-font #:size 40 #:family 'modern #:weight 'bold))
  (send dc set-text-foreground "black")
  (send dc draw-text "Battle Tetris" 240 15))

;; Om någon förlorar visas text på vinnare/förlorare.
(define (draw-winner-text canvas dc)
  (cond ((send *board-1* lost-game?)  ;; om spelplan-1 inte är "in-game"
         (send dc set-font (make-font #:size 40 #:family 'roman #:weight 'bold))
         (send dc set-text-foreground "red")
         (send dc draw-text "You lose!" 520 60)
         (send dc set-text-foreground "blue")
         (send dc draw-text "You win!" 120 60))
        ((send *board-2* lost-game?)  ;; om spelplan-1 inte är "in-game"
         (send dc set-font (make-font #:size 40 #:family 'roman #:weight 'bold))
         (send dc set-text-foreground "red")
         (send dc draw-text "You lose!" 120 60)
         (send dc set-text-foreground "blue")
         (send dc draw-text "You win!" 520 60))
        (else void)))

;; Visar score. (används till spelbrädens score och high-score)
;; Inargument: canvas dc. Poäng som nummer "score". Texten före poäng "text" (tex "High score: ")
(define (draw-score canvas dc score x y text)
  (let ((score-text (if (eof-object? score)
                          text
                          (string-join (append (list text) (list score))))))
    (send dc set-font (make-font #:size 20 #:family 'roman #:weight 'bold))
    (send dc set-text-foreground "black")
    (send dc draw-text score-text x y)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Övrigt
;;;;;;;;;;;;;;;;;;;;;;;;

;; Allt som ska ritas stoppas här. Kallar på olika procedurer beroende på om det är singel- eller multiplayer 
(define (draw-cycle canvas dc)
  (draw-score canvas dc (call-with-input-file "high-score.data"
                          (lambda (score)
                            (read-string 5 score))) 30 20 "High score: ")
  (draw-game-title canvas dc)
  (cond ((send *board-1* singelplayer?)
         (show-board-1 canvas dc))
        ((send *board-2* singelplayer?)
         (show-board-2 canvas dc))
        (else
         (show-board-1 canvas dc)
         (show-board-2 canvas dc)
         (draw-winner-text canvas dc))))

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
           (send cur-block reset-block) ;; ställer tillbaka startkoordinater på blocket
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
       [parent *game-area*]
       [paint-callback draw-cycle]
       [keyboard-handler (lambda (key-event) ;; Vid knapptryck skickas vissa key-events till en lista. Är aktiva tills de tas bort vid 'release.
                           (let ((key-code (send key-event get-key-code))
                                 (key-code-release (send key-event get-key-release-code)))
                             (if (not (equal? key-code 'release))
                                 (cond ((or (send *board-1* direction-key? key-code) (send *board-2* direction-key? key-code))
                                        (send *board-1* add-active-key key-code)
                                        (send *board-2* add-active-key key-code)
                                        (key-list-to-move (send *board-1* get-active-keys) *board-1*)
                                        (key-list-to-move (send *board-2* get-active-keys) *board-2*))
                                       (else (move key-code *board-1*)
                                             (move key-code *board-2*)))
                                 (begin (send *board-1* remove-active-key key-code-release)
                                        (send *board-2* remove-active-key key-code-release)))))]))
                                 
(send *game-canvas* focus) ;; gör att tangentbordshändelser har "fokus" på *game-canvas*

(provide (all-defined-out))