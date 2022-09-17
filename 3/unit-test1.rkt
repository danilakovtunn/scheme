#lang racket/base
 
(require rackunit "list-fib-sq-and-process.rkt")

(check-equal? (list-fib-squares-a 1) '(1) "")
(check-equal? (list-fib-squares-a 3) '(1 1 4) "")
(check-equal? (list-fib-squares-a 5) '(1 1 4 9 25) "")
(check-equal? (list-fib-squares-a 0) '() "")

(check-equal? (list-fib-squares-b 1) '(1) "")
(check-equal? (list-fib-squares-b 3) '(1 1 4) "")
(check-equal? (list-fib-squares-b 5) '(1 1 4 9 25) "")
(check-equal? (list-fib-squares-b 0) '() "")

(check-equal? (process '((5) (1 2) () (3 4) (2 3) (2 3 4))) '((3 4) (2 3 4)) "")
(check-equal? (process '(())) '() "")
(check-equal? (process '(() (0) (1) (2))) '((2)) "")
(check-equal? (process '((-1) (0))) '((0)) "")