#lang racket
(require "block.rkt")
(require "board.rkt")
(require "cmd_store.rkt")

(define *I*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (7 1))]
       [type 'I]))

(define *J*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (6 2))]
       [type 'J]))

(define *L*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (4 2))]
       [type 'L]))

(define *O*
  (new block%
       [coordinates '((4 1) (5 1) (4 2) (5 2))]
       [type 'O]))

(define *S*
  (new block%
       [coordinates '((5 1) (6 1) (4 2) (5 2))]
       [type 'S]))

(define *T*
  (new block%
       [coordinates '((4 1) (5 1) (6 1) (5 2))]
       [type 'T]))

(define *Z*
  (new block%
       [coordinates '((4 1) (5 1) (5 2) (6 2))]
       [type 'Z]))

(define *board-1*
  (new board%
       [matrix (list (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0 0 0)
                     (list 1 1 1 1 1 1 1 1 1 1)
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
