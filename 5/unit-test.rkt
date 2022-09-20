#lang racket/base
 
(require rackunit "task-5.rkt")

(check-equal? (task-5 #() 0) #t "")
(check-equal? (task-5 #(1 #() #()) 1) #t "")
(check-equal? (task-5 #() 1) #f "")
(check-equal? (task-5 #(1 #(2 #(4 #() #()) #()) #(3 #() #())) 5) #f)
(check-equal? (task-5 #(1 #(2 #(4 #() #()) #()) #(3 #() #())) 3) #f)
(check-equal? (task-5 #(1 #(2 #(4 #() #()) #()) #(3 #() #())) 2) #f)
(check-equal? (task-5 #(1 #(2 #(4 #() #()) #()) #(3 #() #())) 1) #f)
(check-equal? (task-5 #(1 #(2 #(4 #() #()) #()) #(3 #() #())) 0) #f)

(display "Все тесты пройдены\n")