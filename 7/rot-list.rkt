#lang scheme/base
(require scheme/mpair)

(define (rot-left! lst)
    (cond
        ((or (null? lst) (null? (mcdr lst)))
            lst)
        (else
            (let ((x (mcar lst)))
                (set-mcar! lst (mcar (mcdr lst)))
                (set-mcdr! lst (mcdr (mcdr lst)))
                (mappend! lst (mlist x))
            ))
    )
)

(define-syntax macros-rot-left!
    (syntax-rules ()
        ((_ lst)
            (cond
                ((or (null? lst) (null? (mcdr lst)))
                    lst)
                (else
                    (let ((x (mcar lst)))
                        (set! lst (mcdr lst))
                        (mappend! lst (mlist x))
                    )
                )
            )
        )
    )
)

; Так как возможна корректная реализация с помощью функции, то неуместно использовать макрос. 

(define l (mlist 1))
(rot-left! l)
(display l)
(display "\n")

(set! l (mlist))
(rot-left! l)
(display l)
(display "\n")

(set! l (mlist 1 2 3))
(rot-left! l)
(display l)
(display "\n")

(set! l (mlist 1 2 3 4 5))
(rot-left! l)
(display l)
(display "\n")


(set! l (mlist 1))
(macros-rot-left! l)
(display l)
(display "\n")

(set! l (mlist))
(macros-rot-left! l)
(display l)
(display "\n")

(set! l (mlist 1 2 3))
(rot-left! l)
(display l)
(display "\n")

(set! l (mlist 1 2 3 4 5))
(macros-rot-left! l)
(display l)
(display "\n")