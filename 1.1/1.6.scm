;; try find difference between if and new-if
(define (func_a)
  (display "a")
)

(define (func_b)
  (display "b")
)

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)
  )
)

(newline)
(display "cond:")
(cond ((= 1 1) (func_a))
      (else (func_b))
)

(newline)
(display "if:")
(if (= 1 1) 
    (func_a)
    (func_b)
)

(newline)
(display "new if:")
(new-if (= 1 1) 
        (func_a)
        (func_b)
)

