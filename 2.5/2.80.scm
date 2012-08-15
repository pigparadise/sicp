(define (=zero? x) (apply-generic '=zero? x))

(put '=zero? '(scheme-number) (lambda (x) (= x 0)))

(define (=zeroal-rat? r)
  (and (= (numer r) 0) (= (denom r) 0))
)

(put '=zero? '(rational) (lambda (r) (=zeroal-rat? r)))

(define (=zeroal-complex? z)
  (and (= (real-part z) 0)
       (= (imag-part z) 0)
  )
)

(put '=zero? '(complex complex) (lambda (z) (=zeroal-complex? z))))