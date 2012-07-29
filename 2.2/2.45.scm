(define (split op_a op_b)
  (define (f painter n)
    (if (= n 0)
      painter
      (let ((smaller (f painter (- n 1))))
        (op_a painter (op_b smaller smaller))
      )
    )
  )
  f
)

