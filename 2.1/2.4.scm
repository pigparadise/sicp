(define (cons x y)
  (lambda (m) (m x y))
)

(define (car z)
  (z (lambda (p q) p))
)

;; (car z) => 
;; (car (lambda (m) (m x y))) => 
;; (car (lambda (m) (m x y))) =>
;; ((lambda (m) (m x y)) 
;;  (lambda (p q) p)) =>
;; ((lambda (p q) p) x y) =>
;; x

(define (cdr z)
  (z (lambda (p q) q))
)

(newline)
(define p (cons 1 2))
(display p)(newline)
(display (car p))(newline)
(display (cdr p))(newline)
