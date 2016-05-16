#lang racket/gui
;(provide *draw-timer*)
(require "block.rkt")
(require "board.rkt")
(require "game-init.rkt")
(require "draw-game.rkt")

;; Ändrar spelet om villkor är uppfyllda. (tex tar bort fulla rader och kollar om block är i toppen).
(define (conditions)  ;; istället för game-loop...
  (let
      ([full-row-pos-b1 (send *board-1* count-to-full-row)] ;; positionen av en full rad (board-1 och -2).
       [full-row-pos-b2 (send *board-2* count-to-full-row)]
       [b1-score (send *board-1* get-score)]
       [b2-score (send *board-2* get-score)])

    ;; Jämför poäng - avgör vinnare
    (define (decide-winner)
      (if (> b1-score b2-score)
          (send *board-2* lose)
          (send *board-1* lose)))
    
    (cond [(send *board-1* too-high?)
           (send *board-2* add-point (/ b1-score 4)) ;; om spelare-1:s block hamnar för högt får sp-2 25% av sp-1:s poäng innan vinnare avgörs.
           (decide-winner)
           (send *fall-timer-b1* stop)
           (send *fall-timer-b2* stop)
           (send *condition-timer* stop)]
          [(send *board-2* too-high?)
           (send *board-1* add-point (/ b2-score 4))
           (decide-winner)
           (send *fall-timer-b2* stop)
           (send *fall-timer-b1* stop)
           (send *condition-timer* stop)]
          
          [(send *board-1* exist-full-row?) 
           (send *board-1* collapse-from full-row-pos-b1) ;; tar bort fulla rader och får övre block att ramla.
           (send *board-1* add-point 10)]
          [(send *board-2* exist-full-row?)
           (send *board-2* collapse-from full-row-pos-b2) ;; tar bort fulla rader och får övre block att ramla.
           (send *board-2* add-point 10)]
          
          [else void])))


(define *draw-timer* (new timer%
                     [notify-callback refresh-draw-cycle]))

(define *fall-timer-b1* (new timer%
                             [notify-callback (lambda ()
                                                (draw-fall *board-1*))]))

(define *fall-timer-b2* (new timer%
                             [notify-callback (lambda ()
                                                (draw-fall *board-2*))]))

(define *condition-timer* (new timer%
                               [notify-callback conditions]))

(send *board-1* queue-block (generate-block *board-1*)) ;; lägger ett slumpat block i "kö"
(send *board-2* queue-block (generate-block *board-2*))
(send *window* show #t)

(define (button-hanler button-press)
  (cond ((equal? button-press "Play multiplayer")
         (send *horizontal-bottom* delete-child play-multi-button) ;; tar bort knappar
         (send *horizontal-bottom* delete-child play-singel-button)
         (send *draw-timer* start 16 #f)
         (send *condition-timer* start 16 #f)
         (send *fall-timer-b1* start 300 #f) ;; startar bara timer hos board-1 och -2
         (send *fall-timer-b2* start 300 #f))
        ((equal? button-press "Play singelplayer")
         (send *horizontal-bottom* delete-child play-multi-button) ;; tar bort knappar
         (send *horizontal-bottom* delete-child play-singel-button)
         (send *board-1* play-singelplayer)
         (send *draw-timer* start 16 #f)
         (send *condition-timer* start 16 #f)         
         (send *fall-timer-b1* start 300 #f))
        ((equal? button-press "QUIT")
         (send *window* show #f))
        (else (void))))

;; Skapar knapp som vid klick skickar sin lable till play-game.
(define (make-button panel button-label)
  (new button%
       [parent panel]
       [label button-label]
       [callback (lambda (button event)
                   (button-hanler (send button get-label)))]
       [font (make-font #:size 20 #:family 'roman #:weight 'bold)]
       ))

;(define *horizontal-center*
;  (new horizontal-panel%
;       [parent *window*]
;       [alignment '(center center)]
;       [style '(border)]))
;
(define *horizontal-bottom*
  (new horizontal-panel%
       [parent *window*]
       [alignment '(center bottom)]
       [min-height 0]
       [style '(border)]
       [border 0]
       [spacing 10]
       ))

(define play-multi-button
  (make-button *horizontal-bottom* "Play multiplayer"))

(define play-singel-button
  (make-button *horizontal-bottom* "Play singelplayer"))

(define quit-button
  (make-button *horizontal-bottom* "QUIT"))