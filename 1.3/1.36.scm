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


(define nsteps 0)
(define (reset_nsteps)
  (set! nsteps 0)
)

(define (inc_nsteps)
 (set! nsteps (+ nsteps 1))
)

(define (term x)
  (inc_nsteps)
  (/ (log 1000))
)

(define (average-damping-term x)
  (inc_nsteps)
  (/ (+ x (term x)) 2)
)

(newline)
(reset_nsteps)
(display "normal: ")(display (fixed-point term 2))
(display " nsteps: ")(display nsteps)
(newline)

(reset_nsteps)
(display "average damping: ")
(display (fixed-point average-damping-term 2))
(display " nsteps: ")(display nsteps)
(newline)


