#lang racket
(require "block.rkt")
(require "board.rkt")

(define *I*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (7 1))]
       [start-coordinates '((4 1) (5 1) (6 1) (7 1))]
       [type 'I]
       [color 1]))

(define *J*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (6 2))]
       [start-coordinates '((4 1) (5 1) (6 1) (6 2))]
       [type 'J]
       [color 2]))

(define *L*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (4 2))]
       [start-coordinates '((4 1) (5 1) (6 1) (4 2))]
       [type 'L]
       [color 3]))

(define *O*
  (new block%
       [coordinates '((4 1) (5 1) (4 2) (5 2))]
       [start-coordinates '((4 1) (5 1) (4 2) (5 2))]
       [type 'O]
       [color 4]))

(define *S*
  (new block%
       [coordinates '((5 1) (6 1) (4 2) (5 2))]
       [start-coordinates '((5 1) (6 1) (4 2) (5 2))]
       [type 'S]
       [color 5]))

(define *Z*
  (new block%
       [coordinates '((4 1) (5 1) (5 2) (6 2))]
       [start-coordinates '((4 1) (5 1) (5 2) (6 2))]
       [type 'Z]
       [color 6]))

(define *T*
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
       [drop-key #\n]))

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
       [drop-key #\r]))

;; Returnerar färg från nummer. Här bestäms vilken färg varje nummer ska ha.
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
(send *board-1* add-all-types *I*)
(send *board-1* add-all-types *J*)
(send *board-1* add-all-types *L*)
(send *board-1* add-all-types *O*)
(send *board-1* add-all-types *S*)
(send *board-1* add-all-types *T*)
(send *board-1* add-all-types *Z*)

(send *board-1* queue-block *I*)  ;; egentligen inte här som vi kommer att sätta in block på kö

(send *board-2* queue-block *I*)

(send *board-2* add-all-types *I*)
(send *board-2* add-all-types *J*)
(send *board-2* add-all-types *L*)
(send *board-2* add-all-types *O*)
(send *board-2* add-all-types *S*)
(send *board-2* add-all-types *T*)
(send *board-2* add-all-types *Z*)

(provide (all-defined-out))
