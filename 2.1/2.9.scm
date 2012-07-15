;; prove add and sub
;; w = (upper - lower) / 2
;; w1 = (upper1 - lower1) / 2
;; w2 = (upper2 - lower2) / 2

;; add-w = ((upper1 + upper2) - (lower1 + lower2)) / 2 = w1 + w2
;; sub-w = ((upper1 - lower2) - (lower1 - upper2)) / 2 = w1 - w2


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

(define (make-interval a b) (cons a b))
(define (lower-bound p) (car p))
(define (upper-bound p) (cdr p))
(define (width p)
  (/ (- (upper-bound p) (lower-bound p)) 2)
)


(define x (make-interval 6.12 7.48))
(define y (make-interval 4.465 4.935))

(newline)
(display (* (width x) (width y)))
(display " != ")
(display (width (mul-interval x y)))

(newline)
(display (/ (width x) (width y)))
(display " != ")
(display (width (div-interval x y)))

