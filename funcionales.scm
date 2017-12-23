(define (fibo n)
    (if (= n 0)
        0
        (if (= n 1)
            1
            (+ (fibo(- n 1)) (fibo(- n 2)))
        )
    )
)

(define (expo b e)
    (if (= e 0)
        1
        (* b (expo b (- e 1)))
    )
)

(define (minimo L)
    (cond
        ((null? (cdr L)) (car L))
        ((< (car L) (minimo (cdr L))) (car L))
        (else (minimo (cdr L)))
    )
)

(define (inserta n L)
    (cond
        ((null? L) (list n))
        ((< n (car L)) (cons n L))
        (else (cons (car L) (inserta n (cdr L))))
    )
)

(define (concatena L1 L2)
    (if (null? L1) 
        L2
        (cons (car L1) (concatena (cdr L1) L2))
    )
)

(define (elimina E L)
    (cond
        ((null? L) '()) 
        ((equal? E (car L)) (elimina E (cdr L)))
        (else (cons (car L) (elimina E (cdr L))))
    )
)

(define (repetidos L)
    (cond
        ((null? L) '()) 
        ((not (esta (cdr L) (car L))) (cons (car L) (repetidos (cdr L))))
        (else (repetidos (cdr L)))
    )
)

(define (esta L E)
    (cond
        ((null? L) #f) 
        ((equal? E (car L)) #t)
        (else (esta (cdr L) E))
    )
)

(define (invierte L)
    (cond
        ((null? L) '()) 
        ((null? (cdr L)) (list (car L)))
        (else (concatena (invierte (cdr L)) (list (car L))))
    )
)