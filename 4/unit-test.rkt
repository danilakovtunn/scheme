#lang racket/base
 
(require rackunit "task-4.rkt")

(check-equal? (task-4 #() 0) #t "")
(check-equal? (task-4 #(1 #() #()) 1) #t "")
(check-equal? (task-4 #() 2) #f "")
(check-equal? (task-4 #(0 #(0 #() #()) #()) 2) #t "")
(check-equal? (task-4 #(0 #(0 #() #()) #(0 #() #())) 2) #f "")
(check-equal? (task-4 #(0 #(0 #(0 #() #()) #()) #(0 #() #())) 3) #t "")

(display "Все тесты пройдены\n")
; (check-equal? (task-4 #() 2) #f "")