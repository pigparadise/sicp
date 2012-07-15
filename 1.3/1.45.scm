(define (fixed-point f first-guess)
  (define tolerance 0.000000001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )

  (define (try guess)
    (let ((next (f guess))
         )
      (if (close-enough? guess next)
          next
          (try next)
      )
    )
  )
  (try first-guess)
)


(define (avg a b) 
  (/ (+ a b) 2)
)

(define (create-average-damp f)
  (lambda (x)
    (avg x (f x))
  )
)

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))
  )
)

(define (create-y x n)
  (lambda (y)
    (/ x (expt y (- n 1)))
  )
)


(define (calc_root x n)
  (fixed-point (create-average-damp (create-y x n))
               1.0
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (repeated f n)
  (lambda (x)
    (define (iter i result)
       (if (< i n)
           (iter (+ i 1) (f result))
           result
       )
     )
     (iter 1 (f x))
  )
)
(define (calc_root-repeated-damp x nroot nrepeat)
  (display "nroot: ")(display nroot)
  (display ", nrepeat: ")(display nrepeat)
  (newline)
  (fixed-point ((repeated create-average-damp nrepeat)
                (create-y x nroot))
               1.0
  )
)


(newline)
(display (calc_root 2 2))(newline)
(display (calc_root 2 3))(newline)
;; (display (calc_root 2 4))(newline)


(display (calc_root-repeated-damp 2 4 2))(newline)
(display (calc_root-repeated-damp 2 5 2))(newline) ;; pass
(display (calc_root-repeated-damp 2 7 2))(newline) ;; pass
;; (display (calc_root-repeated-damp 2 8 2))(newline) ;; fail
(display (calc_root-repeated-damp 2 8 3))(newline) ;; pass

;; guess nroot < 2^(nrepeat+1)
(display (calc_root-repeated-damp 2 15 3))(newline) ;; pass
;; (display (calc_root-repeated-damp 2 16 3))(newline) ;; fail
(display (calc_root-repeated-damp 2 16 4))(newline) ;; pass

(display (calc_root-repeated-damp 2 31 4))(newline) ;; pass
;; (display (calc_root-repeated-damp 2 32 4))(newline) ;; fail


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; so the final rev:
(define (final-calc-root x nroot)
  (display "nroot: ")(display nroot)

  (define nrepeat (+ 1 
                     (floor (/ (log nroot) (log 2) )
                     )
                  )
  )
  (display ", nrepeat: ")(display nrepeat)
  (newline)

  (fixed-point ((repeated create-average-damp nrepeat)
                (create-y x nroot))
               1.0
  )
)

(display (final-calc-root 2 31))(newline)
(display (final-calc-root 2 32))(newline)
(display (final-calc-root 2 63))(newline)
(display (final-calc-root 2 64))(newline)
(display (final-calc-root 2 127))(newline)
(display (final-calc-root 2 128))(newline)