#lang racket/base

; 2.I
(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))

; Является ли дерево деревом фиббоначи высоты h
(define (isfib? tree h)
    (let helper ((tree tree) (h h))
        (cond 
            ((and (empty-tree? tree) (not (= h 0))) #f)
            ((and (empty-tree? tree) (= h 0)) #t)
            ((and (empty-tree? (tree-left tree)) (empty-tree? (tree-right tree)) (= h 1)) #t)
            ((and (not (empty-tree? (tree-left tree))) (>= (tree-data (tree-left tree)) (tree-data tree))) #f)
            ((and (not (empty-tree? (tree-right tree))) (<= (tree-data (tree-right tree)) (tree-data tree))) #f)
            (else
                (or  
                    (and (helper (tree-left tree) (- h 1)) (helper (tree-right tree) (- h 2)))
                    (and (helper (tree-right tree) (- h 1)) (helper (tree-left tree) (- h 2)))
                )
            )
        )
    )
)

; Поиск высоты дерева
(define (tree-depth tree)
    (let helper ((tree tree) (h 0))
        (if (empty-tree? tree)
            h
            (max
                (helper (tree-left tree) (+ h 1))
                (helper (tree-right tree) (+ h 1))
            )
        )
    )
)

(define (fun2-i tree)
    (isfib? tree (tree-depth tree))
)

; (fun2-i #())
; (fun2-i (vector 0 (vector -1 #() #()) (vector 4 (vector 3 #() #()) #())))
; (fun2-i #(7 #(5 #(4 #() #()) #(6 #() #())) #()))
; (fun2-i #(7 #(6 #(3 #() #()) #(9 #() #())) #(9 #() #())))


; 2.II
; Присутствует ли элемент в списке
(define (inList? elem lst)
    (call/cc 
        (lambda (cc-exit)
            (foldl
                (lambda (x y)
                    (if (= elem x)
                        (cc-exit #t)
                        #f))
                #f
                lst
            )
        )
    )
)

(define (f-2-ii lst)
    (car 
        (foldl
            (lambda (x y) 
                (if (inList? x (cdr y))
                    y
                    (cons (add1 (car y)) (cons x (cdr y)))))
            (cons 0 '())
            lst
        )
    )
)


; (f-2-ii (list 1 2 1 3 1 4 1 1 1 4 4 4 6 6 6))

; 2.III
(require racket/stream)

(define stream2-3
    (let gen ((elem 3) (a 0) (b 1) (c 1) (d 2) (e 4))
        (if (and (> elem d) (< elem e)) 
            (stream-cons elem (gen (add1 elem) a b c d e))
            (stream-cons (add1 elem) (gen (+ elem 2) b c d e (+ a b c d e)))
        )
    )
)


; 2.IV
(define (fun-2-iv f)
    (let ((counter 0))
        (define (new-f x)
            (cond 
                ((eq? x 'get-counter) counter)
                ((eq? x 'zero-count) (set! counter 0) #t)
                (else
                    (set! counter (add1 counter)) (f x)                
                )
            )
        )
        new-f
    )
)

; 2.VI
; (λx. ((λz. (λy. (z x))) x ((λy. y y) (λx. x x)))) y z     -> по Alpha ->
; (λx. ((λq. (λr. (q x))) x ((λw. w w) (λv. v v)))) y z     -> по Beta ->
; ((λq. (λr. (q y))) y ((λw. w w) (λv. v v))) z             -> по Beta ->
; ((λr. (y y)) ((λw. w w) (λv. v v))) z                     -> по Beta ->
; y y z                                                     Дальше нельзя применить Beta-редукции => найдена нормальная форму
