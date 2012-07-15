(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))
  )
)

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y)))
       )
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
  )
)

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))
                )
  )
)

;; 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound p) (car p))
(define (upper-bound p) (cdr p))


(define x (make-interval 6.12 7.48))
(define y (make-interval 4.465 4.935))

(newline)(display (add-interval x y))
(newline)(display (mul-interval x y))
(newline)(display (div-interval x y))