(define (iterative-improve improve good-enough? first-guess)
  (define (try guess)
    (let ((next (improve guess))
         )
      (display "guess: ")(display guess)
      (display ", next: ")(display next)(newline)
      (if (good-enough? guess next)
          next
          (try next)
      )
    )
  )
  (try first-guess)
)

(define (fixed-point improve first-guess)
  (define tolerance 0.000001)
  (define (good-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )

  (iterative-improve improve good-enough? first-guess)
)


(define (sqrt x)
  (define tolerance 0.00001)
  (define (good-enough? guess next)
    (< (abs (- (square next) x)) tolerance)
  )

  (define (improve guess)
    (/ (+ guess (/ x guess)) 2)
  )

  (iterative-improve improve good-enough? 1.0)
)


(newline)
(display (sqrt 2))(newline)

(define (improve y)
  (/ (+ y (/ 2 (square y))) 2)
)
(display (fixed-point improve 1.0))(newline)