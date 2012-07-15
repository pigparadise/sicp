(define zero 
        (lambda (f) (lambda (x) x))
)

(define (add-1 n)
        (lambda (f) (lambda (x) 
                            (f ((n f) 
                                x
                               )
                            )
                    )
        )
)

;; (add-1 zero)
(define one
        (lambda (f) (lambda (x) (f x)))
)


;; (add-1 one)
(define two
        (lambda (f) (lambda (x) (f (f x))))


;; (add a b)
(define (add a b)
        (lambda (f) (lambda (x)
                            (f ((a f) 
                                x
                            )
                            )
                    )
        )
)


(define (add-0 a b)
        (lambda (f) (lambda (x)
                            ((a f) 
                                x
                            )
                    )
        )
)


