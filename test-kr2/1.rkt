#lang scheme/base

(require racket/promise)


; 1
(define-syntax swap
    (syntax-rules ()
        ((_ a b)
            (begin 
                (display a)
                (display b))
)))


(swap (begin (display "QQRQ") 500) (begin (display "qq") 100) )
(newline) (newline)
; resolution order?????


; 2

(define x 
    (if (delay (or)) 
        (quotient 7 2)
        (/ 1 0) 
    )
)
x
(promise? (delay (or)))

(newline)

; 3
(length '(1 2 3))
(define lst '())
(cond ((null? lst) #t) (else (null? (cdr lst))))
(newline)

; 4
; lambda-reduction??????


; 6
; (set! qqq 100500) ; error define need
; ????

; 7
(define a 3)
(define b 100)
((lambda (a b) (set! b a)) a b)
a
b

(newline)

; 8
; ???????

; 10
; eazy

; 11
; check

; 12 
; ?????