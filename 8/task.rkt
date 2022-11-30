#lang racket/base
(require racket/stream)

(define (stream-scale s f)
    (stream-map (lambda (x) (* x f)) s)
)

(define (powers x)
    (stream-cons 1 (stream-scale (powers x) x))
)

(define (total-powers-from s1 s2)
    (let ((first1 (stream-first s1)) (first2 (stream-first s2)))
        (if (< first1 first2)
            (stream-cons first1 (total-powers-from (stream-rest s1) s2))
            (stream-cons first2 (total-powers-from s1 (stream-rest s2)))
        )
    )
)
(define total-powers (stream-cons 1 (total-powers-from (stream-rest (powers 2)) (stream-rest (powers 3)))))

(stream-first total-powers)
(stream-first (stream-rest total-powers))
(stream-first (stream-rest (stream-rest total-powers)))
(stream-first (stream-rest (stream-rest (stream-rest total-powers))))
(stream-first (stream-rest (stream-rest (stream-rest (stream-rest total-powers)))))
(stream-first (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest total-powers))))))
(stream-first (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest total-powers)))))))
(stream-first (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest total-powers))))))))
(stream-first (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest total-powers)))))))))
(stream-first (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest total-powers))))))))))