(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))
  )
)

(define (equal? a b)
  (cond ((and (pair? a) (pair? b) (= (length a) (length b)))
         (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
        )

        (else (eq? a b))
  )
)


(newline)

(display (equal? '(this is a list) '(this is a list)))
(newline)

(display (equal? '(this is a list) '(this (is a) list)))
(newline)

(display (equal? 'a 1))
(newline)

(display (equal? (list) (list)))


