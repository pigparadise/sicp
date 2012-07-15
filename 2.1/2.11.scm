;; (lower upper) => (-, -) (-, +) (+, +) 3 kinds
;; 3 * 3 = 9

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y))
       )
    (cond ((and (< lx 0) (< ux 0) (< ly 0) (< uy 0))
           (make-interval(* lx ly) (* ux uy))
          )
          
          ((and (< lx 0) (< ux 0) (< ly 0) (>= uy 0))
           (make-interval (* (min lx ly) uy) (* lx ly))
          )

          ((and (< lx 0) (< ux 0) (>= ly 0) (>= uy 0))
           (make-interval (* lx uy) (max (* lx ux) (* ly uy)))
          )

          ;; not finish the other 6 kinds, just like above
          (else (display "not finish"))
    )
  )
)

(define (make-interval a b) (cons a b))
(define (lower-bound p) (car p))
(define (upper-bound p) (cdr p))

(define x (make-interval -6.12 -7.48))
(define y (make-interval -4.465 -4.935))

(newline)(display (mul-interval x y))

