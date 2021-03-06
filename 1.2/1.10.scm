(define (A x y)
  (cond ((= y 0) 0)
       ((= x 0) (* 2 y))
       ((= y 1) (* 2 y))
       (else (A (- x 1)
                (A x (- y 1))
             )
       )
  )
)

(newline)
(display (A 1 10))(newline)
;; (* 2 (A 1 9))
;; (* 2^2 (A 1 8)))
;; ...
;; (* 2^9 A(1 1))
;; 2^10
;; (A 1 n) -> 2^n

(display (A 2 4))(newline)
;; (A 1 (A 2 3))
;; 2^(A 2 3)
;; 2^(2^(A 2 2))
;; 2^(2^(2^(A 2 1)))
;; 2^16
;; (A 2 n) -> 2^(2^(2^(...(2^2)) n count

(display (A 3 3))(newline)
;; (A 2 (A 2 3))
;; 2^(A 2 3)
;; 2^(2^(2^2))
;; 2^16
;; (A k n) -> 2^(2^(2^(...(2^2)) n+k count
