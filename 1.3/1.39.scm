(define (term-n x i)
  (if (= i 1)
      x
      (square x)
  )
)

(define (term-d i)
  (- (* 2 i) 1)
)


;; iter
(define (iter-tan-cf x n)
  (define (iter i result)
    (if (> i 0)
        (iter (- i 1)
              (/ (term-n x i)
                 (- (term-d i) result)
              )
        )
        result
    )
  )
  (iter n 0)
)


;; recur
(define (tan-cf x n)
  (define (step i)
    (if (= n i)
        0
        (/ (term-n x i)
           (- (term-d i) (step (+ i 1)))
        )
    )
  )
  (step 1)
)


(define (test_calc_tan x n)
  (define (arc x)
    (* 3.1415926535 (/ x 180))
  )

  (display "angle: ")(display x)
  (display ", arc: ")(display (arc x))
  (display ", n: ")(display n)
  (display ", result: ")
  (display 
   ;; (iter-tan-cf (arc x) n)
   (tan-cf (arc x) n)
  )

  (newline)
)

(newline)
(define x 45)
(test_calc_tan x 10)
(test_calc_tan x 100)
(test_calc_tan x 1000)
(test_calc_tan x 10000)

