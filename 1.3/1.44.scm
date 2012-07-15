(define dx 0.00000001)
(define (smooth f)
  (lambda (x)
    ( / 
      (+ (f x) 
         (f (+ x dx))
         (f (- x dx)))
      3)
  )
)

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


(
 ((repeated smooth 10) (lambda (x) (* x x x x x)))
 2
)
