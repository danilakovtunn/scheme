#lang racket/base
 
(require rackunit "coplanar.rkt")

(check-equal? (coplanar? 2 4 -1 8 -10 5 5 -3 2) #t "")
(check-equal? (coplanar? 0 0 1 0 1 0 1 0 0) #f "")