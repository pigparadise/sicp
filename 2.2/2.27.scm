(define (reverse seq)
  (define (iter src dst)
    (if (null? src)
        dst
        (iter (cdr src) (cons (car src) dst))
    )
  )
  (iter seq ())
)

(define (deep-reverse seq)
  (define (iter src dst)
    (cond ((null? src) dst)

          ((pair? (car src))
           (iter (cdr src) (cons (deep-reverse (car src)) dst))
          )

          (else 
           (iter (cdr src) (cons (car src) dst))
          )
    )
  )
  (iter seq ())
)

(define x (list (list 1 2 5) (list 3 4)))

(newline)
(display x)(newline)
(display (reverse x))(newline)
(display (deep-reverse x))
