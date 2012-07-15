(define (make-rat n d)
  (define (gcd a b)
    (if (= 0 b)
        a
        (gcd b (remainder a b))
    )
  )
  
  (let ((n (* (/ d (abs d)) n))
        (d (abs d))
        (g (abs (gcd n d)))
       )
    (cons (/ n g) (/ d g))
  )
)

(newline)
(display (make-rat 6 2))(newline)
(display (make-rat 4 -2))(newline)
(display (make-rat -12 -6))(newline)

