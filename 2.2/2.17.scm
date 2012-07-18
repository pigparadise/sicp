(define (last-pair seq)
  (let ((remains (cdr seq))
       )
    (if (null? remains)
        (car seq)
        (last-pair remains)
    )
  )
)

(last-pair (list 23 72 149 34 "a"))

