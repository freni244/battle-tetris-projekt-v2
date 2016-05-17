#lang racket
(require "block.rkt")
(require "board.rkt")

(define *I-b1*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (7 1))]
       [start-coordinates '((4 1) (5 1) (6 1) (7 1))]
       [type 'I]
       [color 1]))

(define *J-b1*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (6 2))]
       [start-coordinates '((4 1) (5 1) (6 1) (6 2))]
       [type 'J]
       [color 2]))

(define *L-b1*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (4 2))]
       [start-coordinates '((4 1) (5 1) (6 1) (4 2))]
       [type 'L]
       [color 3]))

(define *O-b1*
  (new block%
       [coordinates '((4 1) (5 1) (4 2) (5 2))]
       [start-coordinates '((4 1) (5 1) (4 2) (5 2))]
       [type 'O]
       [color 4]))

(define *S-b1*
  (new block%
       [coordinates '((5 1) (6 1) (4 2) (5 2))]
       [start-coordinates '((5 1) (6 1) (4 2) (5 2))]
       [type 'S]
       [color 5]))

(define *Z-b1*
  (new block%
       [coordinates '((4 1) (5 1) (5 2) (6 2))]
       [start-coordinates '((4 1) (5 1) (5 2) (6 2))]
       [type 'Z]
       [color 6]))

(define *T-b1*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (5 2))]
       [start-coordinates '((4 1) (5 1) (6 1) (5 2))]
       [type 'T]
       [color 7]))

(define *I-b2*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (7 1))]
       [start-coordinates '((4 1) (5 1) (6 1) (7 1))]
       [type 'I]
       [color 1]))

(define *J-b2*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (6 2))]
       [start-coordinates '((4 1) (5 1) (6 1) (6 2))]
       [type 'J]
       [color 2]))

(define *L-b2*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (4 2))]
       [start-coordinates '((4 1) (5 1) (6 1) (4 2))]
       [type 'L]
       [color 3]))

(define *O-b2*
  (new block%
       [coordinates '((4 1) (5 1) (4 2) (5 2))]
       [start-coordinates '((4 1) (5 1) (4 2) (5 2))]
       [type 'O]
       [color 4]))

(define *S-b2*
  (new block%
       [coordinates '((5 1) (6 1) (4 2) (5 2))]
       [start-coordinates '((5 1) (6 1) (4 2) (5 2))]
       [type 'S]
       [color 5]))

(define *Z-b2*
  (new block%
       [coordinates '((4 1) (5 1) (5 2) (6 2))]
       [start-coordinates '((4 1) (5 1) (5 2) (6 2))]
       [type 'Z]
       [color 6]))

(define *T-b2*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (5 2))]
       [start-coordinates '((4 1) (5 1) (6 1) (5 2))]
       [type 'T]
       [color 7]))


(define *board-1*
  (new board%
       [matrix (list (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0))]
       [left-key 'left]
       [right-key 'right]
       [down-key 'down]
       [rotate-right-key #\space]
       [rotate-left-key #\m]
       [drop-key #\n]
       [direction-keys '(left right down)]))

(define *board-2*
  (new board%
       [matrix (list (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0))]
       [left-key #\a]
       [right-key #\d]
       [down-key #\s]
       [rotate-right-key #\e]
       [rotate-left-key #\q]
       [drop-key #\r]
       [direction-keys '(#\a #\d #\s)]))

;; Returnerar färg från nummer.
(define (return-color-from-num num)
  (cond ((= num 1) "lime")
        ((= num 2) "blue")
        ((= num 3) "red")
        ((= num 4) "yellow")
        ((= num 5) "orange")
        ((= num 6) "cyan")
        ((= num 7) "magenta")
        (else "white"))) ;; beroende på vilken bakgrundsfärgen ska vara. "DodgerBlue"

;; Lägger till alla typer av block (som objekt) i *board-1*.
(send *board-1* add-all-types *I-b1*)
(send *board-1* add-all-types *J-b1*)
(send *board-1* add-all-types *L-b1*)
(send *board-1* add-all-types *O-b1*)
(send *board-1* add-all-types *S-b1*)
(send *board-1* add-all-types *T-b1*)
(send *board-1* add-all-types *Z-b1*)

;(send *board-1* queue-block *I-b1*)  ;; egentligen inte här som vi kommer att sätta in block på kö

;(send *board-2* queue-block *I-b2*)

(send *board-2* add-all-types *I-b2*)
(send *board-2* add-all-types *J-b2*)
(send *board-2* add-all-types *L-b2*)
(send *board-2* add-all-types *O-b2*)
(send *board-2* add-all-types *S-b2*)
(send *board-2* add-all-types *T-b2*)
(send *board-2* add-all-types *Z-b2*)

(provide (all-defined-out))
