#lang racket/base

(define (task-03 lst)
    (sqrt
        (foldl
            +
            0
            (map 
                (lambda (x) 
                    (
                        (lambda (y) (* y y))
                        (/ 
                            (foldl + 0 x) 
                            (length x)
                        )
                    )
                ) 
                lst
            )
        )
    )
)

(provide task-03)