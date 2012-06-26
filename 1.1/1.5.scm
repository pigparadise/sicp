(define x 0)
(define (p x)
  (display x)(newline)
  (p (+ x 1))
);
(define (test x y)
  (if (= x 0)
    0
    y
  )
)

(test 0 (p 0))