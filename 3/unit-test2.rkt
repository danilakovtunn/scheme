#lang racket/base
 
(require rackunit "task.rkt")

(check-equal? (task-03 (list (list 2 4 0) (list 2 2) (list 1 1 1 1 1 1 1))) 3 "")
(check-equal? (task-03 (list (list 1 -1 0) (list 3 3 3) (list 4))) 5 "")
(check-equal? (task-03 (list (list 2 2 2 2))) 2 "")