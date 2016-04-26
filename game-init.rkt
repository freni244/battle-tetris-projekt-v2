#lang racket
(require "block.rkt")
(require "board.rkt")
;(require "movement-and-cmd.rkt")


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
                     (list 1 1 1 1 1 1 1 1 1 0)
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

(provide (all-defined-out))
