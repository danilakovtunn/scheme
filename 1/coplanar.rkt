#lang racket/base

(define (coplanar? x0 y0 z0 x1 y1 z1 x2 y2 z2)
    (define (laplas a11 a22 a23 a32 a33) 
        (* 
            a11 
            (- 
                (* a22 a33) 
                (* a23 a32)
            )
        )
    ) 
    (= 
        (+ 
            (- 
                (laplas x0 y1 z1 y2 z2) 
                (laplas y0 x1 z1 x2 z2)
            ) 
            (laplas z0 x1 y1 x2 y2)
        ) 
        0
    )
)

(provide coplanar?)