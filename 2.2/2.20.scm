(define (same-parity first . others)
  (define (iter src dst filter)
    (display src)(newline)
    (cond ((null? src) dst)

          ((filter (car src))
           (cons (car src)
                 (iter (cdr src) dst filter)
           )
          )

          (else (iter (cdr src) dst filter)
          )
          
    )
  )

  (iter others () 
        (lambda (x) (even? (+ first x)))
  )
)

(newline)
(display (same-parity 1 2 3 4 5 6 7))
(newline)
(newline)
(display (same-parity 2 3 4 5 6 7))