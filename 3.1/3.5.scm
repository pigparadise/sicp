(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) 
           (/ trials-passed trials)
          )
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1))
          )
          (else 
           (iter (- trials-remaining 1) trials-passed)
          )
    )
  )
  (iter trials 0)
)


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))
  )
)


(define r 3.0)
(define (estimate-integral p x1 x2 y1 y2 ntrials)
  (define (exp)
    (p (random-in-range x1 x2) (random-in-range y1 y2))
  )
  (define (area)
    (abs (* (- x1 x2) (- y1 y2)))
  )
  ;; need div r^2
  (/ (* (area) (monte-carlo ntrials exp)) (square r))
)

(newline)
(define (inside x y)
  (<=
   (+ (square (- x 5.0)) (square (- y 7.0)))
   (square r)
  )
)

(estimate-integral inside 2.0 8.0 4.0 10.0 1000000) 
