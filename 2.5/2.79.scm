(define (equ? a b) (apply-generic 'equ? a b))

(put 'equ? '(scheme-number scheme-number)
     (lambda (a b) (= a b))
)

(define (equal-rat? a b)
  (and (= (numer a) (numer b)) (= (denom a) (denom b)))
)

(put 'equ? '(rational rational)
     (lambda (a b) (equal-rat? a b))
)

(define (equal-complex? z1 z2)
  (and (= (real-part z1) (real-part z2))
       (= (imag-part z1) (imag-part z2))
  )
)

(put 'equ? '(complex complex)
     (lambda (z1 z2) (equal-complex? z1 z2)))
)