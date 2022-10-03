#lang scheme/base

(define (taskI lst)
    (if (null? lst)
        '()
        (car 
            (foldl
                (lambda (x y) (
                    cond
                        ((< x (caddr y)) (list (list (cadr y)) (add1 (cadr y)) x ))
                        ((= x (caddr y)) (list (cons (cadr y) (car y)) (add1 (cadr y)) x))
                        (else (list (car y) (add1 (cadr y)) (caddr y)))
                    
                ))
                (list '() 0 +inf.0)
                lst
            )
        )
    )
)


(define (taskII t s)
    (cond
        ((and (vector? t) (= (vector-length t) 1)) (* (vector-ref t 0) s))
        ((equal? t 1) s)
        ((equal? t 0) 0)
        (else 
            (+ 
                (taskII (vector-ref t 0) (/ s 4))
                (taskII (vector-ref t 1) (/ s 4))
                (taskII (vector-ref t 2) (/ s 4))
                (taskII (vector-ref t 3) (/ s 4))
            )
        )
    )
)


(define (taskIII t)
    (cond 
        ((and (vector? t) (= (vector-length t) 1) (= (vector-ref t 0) 1)) #t)
        ((and (vector? t) (= (vector-length t) 1) (= (vector-ref t 0) 0)) #f)
        (else
            (call/cc 
                (lambda (cc-exit) 
                    (let helper ((vec t) (s 1) (res (list 0 0)))
                        (begin 
                        (printf "~v ~v \n" vec res) 
                        (cond
                            ((> (car res) 0.5) (cc-exit #t))
                            ((> (cadr res) 0.5) (cc-exit #f))
                            ((and (vector? vec) (null? (vector->list vec)) (= s 1)) (> (car res) (cadr res)))
                            ((and (vector? vec) (null? (vector->list vec))) res)
                            ((equal? vec 1) (map + (list s 0) res))
                            ((equal? vec 0) (map + (list 0 s) res))
                            (else 
                                (helper 
                                    (list->vector (cdr (vector->list vec)))
                                    s
                                    (helper (vector-ref vec 0) (/ s 4) res)
                                )
                            )
                        )
                        )
                    )
                )
            )
        )
    )
)

(define (taskIV-cc t s cc)
    (cond
        ((and (vector? t) (= (vector-length t) 1)) (cc (* (vector-ref t 0) s)))
        ((equal? t 1) (cc s))
        ((equal? t 0) (cc 0))
        (else 
            (taskIV-cc (vector-ref t 0) (/ s 4) (lambda(x)
            (taskIV-cc (vector-ref t 1) (/ s 4) (lambda(y)
            (taskIV-cc (vector-ref t 2) (/ s 4) (lambda(z)
            (taskIV-cc (vector-ref t 3) (/ s 4) (lambda(w)
            (cc (+ x y z w))))))))))
        )
    )
)


(define (taskV . params)
    (foldl (lambda(x y)(lambda(z)(x (y z)))) (car params) (cdr params))
)
