#lang scheme/base

(define (make-queue)
    (mcons 'queue '())
)

(define (front-queue q)
    (if (and (queue? q) (not (empty-queue? q)))
        (let loop ((head q))
            (if (null? (mcdr head))
                (mcar head)
                (loop (mcdr head))
            )
        )
        "queue error"
    )
)

(define (insert-queue! q e)
    (if (queue? q)
        (set-mcdr! q (mcons e (mcdr q)))
        "queue error"
    )
)

(define (delete-queue! q)
    (if (and (queue? q) (not (empty-queue? q)))
        (let loop ((head q))
            (if (null? (mcdr (mcdr head)))
                (set-mcdr! head '())
                (loop (mcdr head))
            )
        )
        q
    )
)

(define (queue? q)
    (and (mpair? q) (equal? 'queue (mcar q)))
)

(define (empty-queue? q)
    (and (queue? q) (null? (mcdr q)))
)