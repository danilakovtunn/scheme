#lang scheme/base
(require math/number-theory)

(define (vector-fold-right func init-val vctr)
    (let loop ((i (sub1 (vector-length vctr))) (res init-val))
        (if (= i -1) 
            res
            (loop (sub1 i) (func i res (vector-ref vctr i)))
        )
    )
)

; (vector-fold-right (lambda (i val el) (cons el val)) '() #())
; (vector-fold-right (lambda (i val el) (cons el val)) '() #(10 20 30))

(define (fun2a n)
    (reverse
        (let loop ((beg 2))
            (cond 
                ((= beg (add1 n)) '()) ; добрались до конца - завершаем
                ((and
                    (= 0 (remainder n beg))
                    (not (prime? beg))
                ) (cons beg (loop (add1 beg)))) ; число просто и составное
                (else (loop (add1 beg))) ; переходим к следующей итерации
            )
        )
    )
)


(define (fun2b n)
    (let loop ((beg 2) (res '()))
        (cond 
            ((= beg (add1 n)) res) ; добрались до конца - завершаем
            ((and
                (= 0 (remainder n beg))
                (not (prime? beg))
            ) (loop (add1 beg) (cons beg res))) ; число просто и составное, добавляем к res число
            (else (loop (add1 beg) res)) ; переходим к следующей итерации
        )
    )
)

;  (fun2a 1) 
;  (fun2a 2)
;  (fun2a 12)
;  (fun2b 1) 
;  (fun2b 2)
;  (fun2b 12)


(define (fun3 n)
    (let loop ((i 0) (res 1))
        (if (= i n) 
            res ; если дошли до нужного номера - возвращаем res
            (loop (add1 i) (* res (nth-prime i))) ; иначе не дошли - вызываем функцию и умножаем res на i-ое простое число
        )
    )
)

; (fun3 0)
; (fun3 4)
; (fun3 10)

(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))


; сначала неитеративный варинат, для того чтобы его было легче переписать в итеративный 
(define (fun4-noncsp tree r1 r2)
    (let loop ((t tree) (mi (min r1 r2)) (ma (max r1 r2)) (h 0))
        (cond 
            ((empty-tree? t) 
                0
            )
            ((and (>= h mi) (<= h ma) (< (tree-data t) 0))
                (+ 
                    (loop (tree-left t) mi ma (add1 h)) 
                    (loop (tree-right t) mi ma (add1 h)) 
                    1
                )
            )
            (else 
                (+ 
                    (loop (tree-left t) mi ma (add1 h)) 
                    (loop (tree-right t) mi ma (add1 h)) 
                )
            )
        )
    )
)

(define (fun4 tree r1 r2)
    (let ((mi (min r1 r2)) (ma (max r1 r2)))
        (let loop ((t tree) (h 0) (cc (lambda(x) x)))
            (cond 
                ((empty-tree? t) 
                    (cc 0)
                )
                ((and (>= h mi) (<= h ma) (< (tree-data t) 0))
                    (loop 
                        (tree-left t) (add1 h) (lambda (x) (
                            loop (tree-right t) (add1 h) (lambda (y) (cc (+ x y 1)))))))
                (else
                    (loop 
                        (tree-left t) (add1 h) (lambda (x) (
                            loop (tree-right t) (add1 h) (lambda (y) (cc (+ x y)))))))
            )
        )
    )
)

; (fun4 #(1 #(-1 #(3 #() #()) #(3 #() #())) #(-2 #() #())) 2 1)
; (fun4 #() 1 10)
; (fun4 #(-10 #() #()) 0 0)

(define (fun5 tree)
    (call/cc
        (lambda (cc-exit)
            (if 
                (let loop ((t tree) (h 0))
                    (cond 
                        ((empty-tree? t) (list h -inf.0))
                        ((or 
                            (> 
                                (abs
                                    (-
                                        (car (loop (tree-right t) (add1 h)))
                                        (car (loop (tree-left t) (add1 h)))
                                    )
                                )
                                1
                            )
                            (> (cadr (loop (tree-right t) (add1 h))) (tree-data t))
                            (> (cadr (loop (tree-left t) (add1 h))) (tree-data t))
                        ) (cc-exit #f))
                        (else
                            (list
                                (max
                                    (car (loop (tree-right t) (add1 h)))
                                    (car (loop (tree-left t) (add1 h)))
                                )
                                (tree-data t)
                            )
                        )   
                    )
                ) #t
                #f
            )
        )
    )
)

; (fun5 #())
; (fun5 #(10 #() #()))
; (fun5 #(1 #(-1 #(3 #() #()) #(3 #() #())) #(-2 #() #())))
; (fun5 #(1 #(-1 #(-3 #() #()) #(-3 #() #())) #(-2 #() #())))