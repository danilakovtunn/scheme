#lang racket/base

(define (2n-1!-list n)
    (cond 
        ((or (not (integer? n)) (< n 1)) '())
        ((= n 1) '(1))
        (else 
            (reverse 
                (let fact_list ((i 2) (curr 1) (result '(1)))
                    (if (> i n)
                        result
                        (fact_list 
                            (+ i 1) 
                            (* curr (-(* 2 i) 2) (- (* 2 i) 1)) 
                            (cons
                                (* curr (-(* 2 i) 2) (- (* 2 i) 1))
                                result 
                            )
                        )
                    )
                )
            )
        )
    )
)

(provide 2n-1!-list)