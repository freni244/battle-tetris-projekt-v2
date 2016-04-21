#lang racket
(provide board%)
(require "block.rkt")

(define board%
  (class object%
    (init-field field)
    (field [next-blocks '()]
           [hold '()])
    
    