#lang racket
(provide block%)

(define block%
  (class object%
    (init-field coordinates
                type)
    (field [hold '()])
  (super-new)))
    