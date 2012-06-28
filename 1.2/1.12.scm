;; n is level, k is index(start from left and with 1)
;; if k == 1 or k == n then f(k, n) = 1
;; else f(k, n) = f(k-1, n-1) + f(k, n-1)

(define (f k n)
  ;; (display "calc:")(display k)(display ",")(display n)(newline)
  (cond 
    ;; ((or (<= k 0) (<= n 0) (> k n)) 0)

    ((or (= k 1) (= k n)) 1)

    (else (+ (f (- k 1) (- n 1))
             (f k (- n 1))
          )
    )
  )
)


(display (f 1 1)) (newline)
(display (f 1 50)) (newline)
(display (f 100 100)) (newline)
(display (f 2 3)) (newline)
(display (f 2 4)) (newline)
(display (f 2 5)) (newline)
(display (f 3 5)) (newline)
(display (f 3 6)) (newline)
(display (f 4 7)) (newline)
(display (f 5 9)) (newline)
(display (f 6 11)) (newline)
