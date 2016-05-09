#lang racket
(require "block.rkt")
(require "board.rkt")

(define *I*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (7 1))]
       [type 'I]
       [color 1]))

(define *J*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (6 2))]
       [type 'J]
       [color 2]))

(define *L*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (4 2))]
       [type 'L]
       [color 3]))

(define *O*
  (new block%
       [coordinates '((4 1) (5 1) (4 2) (5 2))]
       [type 'O]
       [color 4]))

(define *S*
  (new block%
       [coordinates '((5 1) (6 1) (4 2) (5 2))]
       [type 'S]
       [color 5]))

(define *T*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (5 2))]
       [type 'T]
       [color 6]))

(define *Z*
  (new block%
       [coordinates '((4 1) (5 1) (5 2) (6 2))]
       [type 'Z]
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
                     (list 0 0 0 0 0 0 0 0 0 0))]))

;(define (generate-block)
;  (let ((blocks '(*I* *J* *L* *O* *S* *T* *Z* )))
;    (car (shuffle blocks))))

;; Lägger till alla typer av block (som objekt) i *board-1*.
(send *board-1* add-all-types *I*)
(send *board-1* add-all-types *J*)
(send *board-1* add-all-types *L*)
(send *board-1* add-all-types *O*)
(send *board-1* add-all-types *S*)
(send *board-1* add-all-types *T*)
(send *board-1* add-all-types *Z*)

(send *board-1* queue-block *I*)  ;; egentligen inte här som vi kommer att sätta in block på kö
(send *board-1* queue-block *J*)
(send *board-1* queue-block *L*)
(send *board-1* queue-block *O*)
(send *board-1* queue-block *S*)
(send *board-1* queue-block *T*)
(send *board-1* queue-block *Z*)


(provide (all-defined-out))
