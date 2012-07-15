(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))
  )
)


(define (make-interval a b) (cons a b))
(define (lower-bound p) (car p))
(define (upper-bound p) (cdr p))


(define x (make-interval 6.12 7.48))
(define y (make-interval 4.465 4.935))
(newline)(display (sub-interval x y))

