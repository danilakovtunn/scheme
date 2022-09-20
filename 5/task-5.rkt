#lang racket/base

(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))

(define (task-5 tree h)
    (call/cc 
        (lambda (cc-exit)
            (let helper ((tree tree) (h h))
                (cond 
                    ((and (empty-tree? tree) (not (= h 0))) (cc-exit #f))
                    ((and (empty-tree? tree) (= h 0)) #t)
                    ((<= h 0) (cc-exit #f))
                    ((and (empty-tree? (tree-left tree)) (empty-tree? (tree-right tree)) (= h 1)) #t)
                    (else (and (helper (tree-left tree) (sub1 h)) (helper (tree-right tree) (sub1 h))))
                )
            )
        )
    )
)



(provide task-5)