(define (expt a b)
  (if (= 0 b)
      1
      (* a (expt a (- b 1)))
  )
)

(define (my-cons x y)
  (* (expt 2 x) (expt 3 y))
)

(define (divide? a b)
  (= 0 (remainder a b))
)

(define (find n d)
  (define (iter x result)
    (if (divide? x d)
        (iter (/ x d) (+ 1 result))
        result
    )
  )
  (iter n 0)
)
(define (my-car n)
  (find n 2)
)

(define (my-cdr n)
  (find n 3)
)

(newline)
(define p (my-cons 2 3))
(display p)(newline)
(display (my-car p))(newline)
(display (my-cdr p))(newline)

