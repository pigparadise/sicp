(define (in x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (in x (cdr set)))
  )
)

(define (add-set x set)
  (if (in x set)
      set
      (cons x set)
  )
)

(define (inter-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((in (car set1) set2)
         (cons (car set1)
               (inter-set (cdr set1) set2)
         )
        )
        (else (inter-set (cdr set1) set2))
  )
)


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else 
         (union-set (add-set (car set2) set1) (cdr set2))
        )
  )
)

(newline)
(display (in 1 '(1 3)))
(newline)

(display (add-set 'a '(1 3)))
(newline)

(display (inter-set '(a b c) '(c d f)))
(newline)

(display (union-set '(a b c) '(c d f)))
(newline)



