#lang racket/base

(define (list-fib-squares-a n)
    (map
        (lambda (x) (* x x))
        (reverse 
            (let fact_list ((i 1) (pred 0) (curr 1) (result '()))
                (if (> i n)
                    result
                    (fact_list 
                        (add1 i)
                        curr 
                        (+ pred curr)
                        (cons curr result)
                    )
                )
            )
        )   
    )
)

(define (list-fib-squares-b n)
    (foldl 
        (lambda (x y) (cons ((lambda (x) (* x x)) x) y))
        '() 
        (let fact_list ((i 1) (pred 0) (curr 1) (result '()))
            (if (> i n)
                result
                (fact_list 
                    (add1 i)
                    curr 
                    (+ pred curr)
                    (cons curr result)
                )
            )
        ) 
    )
)

(define (process lst)
    (let ((first (foldl * 1 (car lst))))
        (filter
            (lambda (x)
                (<                
                    first
                    (foldl + 0 x)
                )
            )
            (cdr lst);
        )
    )
)

(provide 
    list-fib-squares-a
    list-fib-squares-b
    process
)