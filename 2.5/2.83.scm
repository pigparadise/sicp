;; from web
(define global-dict (make-hash-table))

(define (put key1 key2 value)
  (hash-table-set! global-dict (list key1 key2) value)
)

(define (get key1 key2)
  (hash-table-ref global-dict (list key1 key2))
)

(define (integer->rational n)
  (make-rational n 1)
)
(put 'raise '(integer)
  (lambda (i) (integer->rational i))
)

(define (rational->real r)
  (make-real (+ 0.0 (/ (numer r) (denom r))))
)

(put 'raise '(rational)
  (lambda (r) (rational->real r))
)

(define (real->complex r)
  (make-complex-from-real-imag r 0)
)

(put 'raise '(real)
  (lambda (r) (real->complex r))
)

(define (raise x)
  (apply-generic 'raise x)
)