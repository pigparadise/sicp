(define (make-segment x y)
  (list (make-vect 0 0) (make-vect x y))
)

(define (start-segment s)
  (car s)
)

(define (end-segment s)
  (cadr s)
)