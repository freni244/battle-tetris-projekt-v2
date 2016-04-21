#lang racket
(require "block.rkt")
(require "board.rkt")
(require "interaction-utils.rkt") ; ...
(require "cmd_store.rkt")

(define *I*
  (new block%
       ;[shape '((- c 1))]