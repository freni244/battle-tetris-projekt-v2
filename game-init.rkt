#lang racket
(require "block.rkt")
(require "board.rkt")
(require "cmd_store.rkt")

(define *I*
  (new block%
       [coordinates '((180 10) (200 10) (220 10) (240 10))] ;'((4 1) (5 1) (6 1) (7 1))]
       [type 'I]))

(define *J*
  (new block%
       [coordinates '((4 0) (5 0) (6 0) (6 1))]
       [type 'J]))

(define *L*
  (new block%
       [coordinates '((4 0) (5 0) (6 0) (4 1))]
       [type 'L]))

(define *O*
  (new block%
       [coordinates '((4 0) (5 0) (4 1) (5 1))]
       [type 'O]))

(define *S*
  (new block%
       [coordinates '((5 0) (6 0) (4 1) (5 1))]
       [type 'S]))

(define *T*
  (new block%
       [coordinates '((4 0) (5 0) (6 1) (5 1))]
       [type 'T]))

(define *Z*
  (new block%
       [coordinates '((4 0) (5 0) (5 1) (6 1))]
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
