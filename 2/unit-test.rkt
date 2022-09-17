#lang racket/base
 
; (require rackunit "list-factorial-rec.rkt")
(require rackunit "list-factorial-iter.rkt")

(check-equal? (2n-1!-list 1) '(1) "")
(check-equal? (2n-1!-list 2) '(1 6) "")
(check-equal? (2n-1!-list 3) '(1 6 120) "")
(check-equal? (2n-1!-list 4) '(1 6 120 5040) "")
(check-equal? (2n-1!-list 5) '(1 6 120 5040 362880) "")
(check-equal? (2n-1!-list 0) '() "")
(check-equal? (2n-1!-list -1) '() "")
(check-equal? (2n-1!-list 1.2) '() "")
(check-equal? (2n-1!-list 0+i) '() "")