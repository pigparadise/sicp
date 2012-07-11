;; iter
(define (iter-accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))
    )
  )
  (iter a null-value)
)

;; recur
(define (recur-accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner
         (term a)
         (recur-accumulate combiner null-value term (next a) next b)
        )
    )
)

(define (sum term a next b)
  (iter-accumulate + 0 term a next b)
)


(define (produce term a next b)
  (iter-accumulate * 1 term a next b)
)


(sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 100)
(produce (lambda (x) x) 1 (lambda (x) (+ x 1)) 5)