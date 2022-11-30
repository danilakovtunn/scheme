#lang racket/base

(define Y 
    (lambda (f) (
        (lambda (x) (x x))
        (lambda (g) (f (lambda args (apply (g g) args)))))
    )
)

; С ОСТАТОЧНЫМИ ВЫЧИСЛЕНИЯМИ, НЕ ИТЕРАТИВНЫЙ ПРОЦЕСС
(define n!!!! (Y (lambda (f) (
    lambda (n) 
        (cond 
            ((< n 2) 1)
            ((= n 2) 2)
            ((= n 3) 3)
            (else
                (* n (f (- n 4)))
            )
        )
    )))
)

; ОТВЕТ (ИТЕРАТИВНЫЙ ПРОЦЕСС)
(define n!!!!v2 (lambda (n)
    ((Y (lambda (f) (lambda (i result) (
        cond 
            ((< i 2) result)
            ((= i 2) (* result 2))
            ((= i 3) (* result 3))
            (else 
                (f (- i 4) (* i result))
            )
        )))) 
        n 1)))

(n!!!!v2 0)
(n!!!!v2 1)
(n!!!!v2 2)
(n!!!!v2 3)
(n!!!!v2 4)
(n!!!!v2 5)
(n!!!!v2 6)
(n!!!!v2 7)
(n!!!!v2 8)
(n!!!!v2 9)
(n!!!!v2 10)
(n!!!!v2 11)