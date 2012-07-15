(define (double func)
  (lambda (x)
    (func (func x))
  )
)

(define (inc x) (+ x 1))

(newline)
(((double (double double)) inc) 5)

;; 2^(2^2) = 16