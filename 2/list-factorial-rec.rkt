#lang racket/base

(define (2n-1!-list n)
    (define (fact_list i curr)
        (if (> i n)
            '()
            (cons 
                (* curr (-(* 2 i) 2) (- (* 2 i) 1))
                (fact_list 
                    (+ i 1) 
                    (* curr (-(* 2 i) 2) (- (* 2 i) 1)) 
                )
            )
        )
    )
    (cond 
        ((or (not (integer? n)) (< n 1)) '())
        ((= n 1) '(1))
        (else 
            (cons 
                1 
                (fact_list 2 1)
            )
        )
    )
)

(provide 2n-1!-list)