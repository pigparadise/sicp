(newline)

(define (cubic-root-iter old_guess new_guess x)
  (display "iter:")(display old_guess)(display ",")(display new_guess)(newline)
  (if (good-enough? old_guess new_guess)
      new_guess
      (sqrt-iter new_guess (improve new_guess x) x)
  )
)

(define (improve guess x)
  (define t ( / 
              ( + 
                (/ x (* guess guess))
                (* 2 guess)
              )
              3
            )
  )
  (display "improve:")(display t)(newline)
  t
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

(define (cubic-root x)
 (cubic-root x 1.0 x)
)

(display (sqrt 27)))