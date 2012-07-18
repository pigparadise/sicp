(define (reverse seq)
  (define (iter src dst)
    (if (null? src)
        dst
        (iter (cdr src) (cons (car src) dst))
    )
  )
  (iter seq ())
)

(reverse (list 1 4 9 16 25 36 49 64 81 100))