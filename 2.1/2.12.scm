(define (make-center-percent i percent)
  (cons (* i (- 100 percent) 0.01)
        (* i (+ 100 percent) 0.01))
)

(define (lower-bound p) (car p))
(define (upper-bound p) (cdr p))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2)
)

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2)
)

(define x (make-center-percent 6.8 10))
(newline)(display x)
(newline)(display (center x))
(newline)(display (width x))