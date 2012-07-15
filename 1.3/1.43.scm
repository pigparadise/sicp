(define (repeated f n)
  (lambda (x)
    (define (iter i result)
      (if (< i n)
          (iter (+ i 1) (f result))
          result
      )
    )
    (iter 1 (f x))
  )
)

((repeated square 2) 4)
