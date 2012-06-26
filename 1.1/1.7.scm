(newline)

(define (sqrt-iter old_guess new_guess x)
  (display "iter:")(display old_guess)(display ",")(display new_guess)(newline)
  (if (good-enough? old_guess new_guess)
      new_guess
      (sqrt-iter new_guess (improve new_guess x) x)
  )
)

(define (improve guess x)
  (define t (average guess (/ x guess)))
  (display "improve:")(display t)(newline)
  t
)

(define (average x y)
  (/ (+ x y) 2)
)

(define (abs x)
  (if (> x 0) x (- x))
)
(define stop_rate 0.001)
(define (good-enough? old_guess new_guess)
  ( <
    ( / 
      (abs (- old_guess new_guess))
      new_guess
    )
    stop_rate
  )
)
;; (define (good-enough? guess x)
;;   (< (abs (- (* guess guess) x)) stop_rate)
;; )

(define (sqrt x)
 (sqrt-iter x 1.0 x)
)

(display (sqrt 500000000000000000)))