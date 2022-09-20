#lang scheme

(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))


(define (print-tree-by-level-desc tree)
    (define (fringe-cps t h cc)
        (cond 
            ((empty-tree? t) 
                (cc '())
            ) 
            ((and (empty-tree? (tree-left t)) (empty-tree? (tree-right t))) 
                (cc (list (cons h (tree-data t))))
            )
            (else 
                (fringe-cps (tree-right t) (add1 h) (lambda (y) (fringe-cps (tree-left t) (add1 h) (lambda (z) (cc (append y z (list(cons h (tree-data t)))))))))
            )
        )
    )
    (if (empty-tree? tree) (void)
        (for-each 
            (lambda (x) (begin 
                (for-each (lambda (y) (printf "~s " (cdr y))) x) 
                (printf "\n"))
            )
            (map 
                (lambda (y) (filter 
                        (lambda (x) (= (car x) y)) 
                        (fringe-cps tree 0 (lambda(x) x)))
                ) 
                (reverse (build-list 
                        (exact-round (add1 (foldl max -inf.0 (map car (fringe-cps tree 0 (lambda(x) x)))))) 
                        values)
                )
            )
        )
    )
)

(print-tree-by-level-desc #())
(print-tree-by-level-desc #(1 #() #()))
(print-tree-by-level-desc #(10 #(21 #() #()) #(22 #() #())))
(print-tree-by-level-desc #(10 #(21 #(31 #() #()) #()) #(22 #(33 #() #()) #(34 #() #()))))