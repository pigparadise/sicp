(define (create)
  (define x 1)
  (lambda (n)
    (set! x (* x n))
    x
  )
)

(newline)
(define f (create))
(define t (f 0))
(display (+ t (f 1)))(newline)

(define f (create))
(define t (f 1))
(display (+ (f 0) t))(newline)