(define (div-interval x y)
  (let ((p1 (lower-bound y))
        (p2 (upper-bound y))
       )
    (if (or (= p1 0) (= p2 0))
        (error "error, can't divide zero")
        (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))
                      )
        )
    )
  )
)

(define (make-interval a b) (cons a b))
(define (lower-bound p) (car p))
(define (upper-bound p) (cdr p))


(define x (make-interval 4.465 4.935))
(define y (make-interval 0 1))

(newline)(display (div-interval x y))
