;; recur
(define (recur-produce f a next b)
  (if (> a b)
      1
      (* (f a)
         (recur-produce f (next a) next b)
      )
  )
)

;; iter
(define (iter-produce f a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (f a)))
    )
  )
  (iter a 1)
)



(define (divide a b)
  (if (even? a)
      (/ a 2)
      (/ (- a 1) 2)
  )
)

(define (inc x) (+ x 1))

(define (float x) (+ 0.0 x))

(define (term k)
  (float 
   (/
    (+ (* 2 (divide (+ k 1) 2)) 2)
    (+ (* 2 (divide k 2)) 3)
   )
  )
)


(define (recur-factorial n)
  (* 4.0 (recur-produce term 0 inc n))
)

(define (iter-factorial n)
  (* 4.0 (iter-produce term 0 inc n))
)


(newline)
(display "recur:")(newline)
(display (recur-factorial 100)) (newline)
(display (recur-factorial 1000)) (newline)
(display (recur-factorial 10000)) (newline)

(display "iter:")(newline)
(display (iter-factorial 100)) (newline)
(display (iter-factorial 1000)) (newline)
(display (iter-factorial 10000)) (newline)
(display (iter-factorial 100000)) (newline)