(define dx 0.000001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx
    )
  )
)


(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))
  )
)


(define tolerance 0.000000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )

  (define (try guess)
    (let ((next (f guess))
         )
      (if (close-enough? guess next)
          next
          (try next)
      )
    )
  )
  (try first-guess)
)

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess)
)

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) 
       (* a x x)
       (* b x)
       c
    )
  )
)

(define (test a b c)
  (display "a:")(display a)
  (display ", b:")(display b)
  (display ", c:")(display c)
  (let (
        (x (newtons-method (cubic a b c) 1) )
       )
      (display ", x:")(display x)
      (display ", f(x) = ") (display ((cubic a b c) x))
  )
  (newline)
)

(newline)
(test 1 1 1)
(test 1 2 3)
(test 3 2 1)
