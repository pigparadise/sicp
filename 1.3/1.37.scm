(define (iter-cont-frac term-n term-d k)
  (define (iter i result)
    (if (> i 0)
        (iter (- i 1)
              (/ (term-n k)
                 (+ (term-d k) result)
              )
        )
        result
    )
  )
  (iter k (/ (term-n k) (term-d k)))
)

(define (recur-cont-frac term-n term-d k)
  (define (g i k)
    (if (= i k)
        (/ (term-n k) (term-d k))
        (/ (term-n i)
           (+ (term-d i) (g (+ i 1) k))
        )
    )
  )
  (g 1 k)
)


(newline)
(display (iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1000))(newline)
(display (iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11))(newline)
(display (iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10))(newline)
(display (iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 8))(newline)

(display (recur-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11))(newline)


;; when k >= 10
