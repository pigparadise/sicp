(define (for-each func seq)
  (cond ((not (null? seq))
         (func (car seq))
         (for-each func (cdr seq))
        )
  )
)

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88)
)