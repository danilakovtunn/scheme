#lang scheme/base

(define (sum_dividers n)
    (if (= n 1)
        1
        (let loop ((curr (quotient n 2)) (res n))
            (if (= curr 1)
                (add1 res)
                (if (= 0 (remainder n curr))
                    (loop (sub1 curr) (+ res curr))
                    (loop (sub1 curr) res)
                )
            )
        )
    )
)


(define (is-excess? n)
    (> (sum_dividers n) (* 2 n))
)

(define (odd-abundant n)
    (let loop ((seq-num 1) (odd-num 1))
        (let ((flag (is-excess? odd-num)))
            (cond 
                ((and (= seq-num n) flag) odd-num)
                (flag (loop (add1 seq-num) (+ odd-num 2)))
                (else (loop seq-num (+ odd-num 2)))
            )
        )
    )
)



(define tbl (make-hash '()))

(define (memo-is-excess? n)
    (let ((elem (hash-ref tbl n #f)))
        (if elem
            elem
            (let ((sum (sum_dividers n)))
                (begin 
                    (hash-set! tbl n (> sum (* 2 n)))
                    (> sum (* 2 n))
                )
            )
        )
    )
)

(define (memo-odd-abundant n)
    
    (let loop ((seq-num 1) (odd-num 1))
        (let ((flag (memo-is-excess? odd-num)))
            (cond 
                ((and (= seq-num n) flag) odd-num)
                (flag (loop (add1 seq-num) (+ odd-num 2)))
                (else (loop seq-num (+ odd-num 2)))
            )
        )
    )
)

(memo-odd-abundant 100)
(memo-odd-abundant 101)