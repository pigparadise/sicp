(define (in x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (in x (cdr set)))
  )
)


(define (add-set x set)
  (cond ((null? set) (cons x '()))
        ((> x (car set))
         (cons (car set) (add-set x (cdr set)))
        )
        ((= x (car set)) set)
        (else (cons x set))
  )
)


(define (inter-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2))
           )
        (cond ((= x1 x2)
               (cons x1 (inter-set (cdr set1) (cdr set2)))
              )
              
              ((< x1 x2)
               (inter-set (cdr set1) set2)
              )

              ((> x1 x2)
               (inter-set set1 (cdr set2))
              )
        )
      )
  )
)


(define (union-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2))
           )
        (cond ((= x1 x2)
               (cons x1 (union-set (cdr set1) (cdr set2)))
              )
              
              ((< x1 x2)
               (cons x1 (union-set (cdr set1) set2))
              )

              ((> x1 x2)
               (cons x1 (union-set set1 (cdr set2)))
              )
        )
      )
  )
)


(newline)
(display (add-set 2 '(1 3)))
(newline)

(display (add-set 3 '(1 2 3 4)))
(newline)

(display (add-set 5 '(1 3)))
(newline)


(newline)
(display (inter-set '(1 2 3) '(2 3 4)))
(newline)

(newline)
(display (union-set '(1 2 3) '(2 3 4)))
(newline)

