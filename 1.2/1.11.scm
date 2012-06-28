(define (fa n)
  (cond ( (< n 3) n )
        ( else 
          (+ (fa (- n 1))
             (* 2 (fa (- n 2)))
             (* 3 (fa (- n 3)))
          )
        )
  )
)


(define (fb n) (f-iter 0 0 0 0 n))
(define (f-iter fn3 fn2 fn1 idx n)
    ;; (display "iter:")
    ;; (display " ")(display fn3)
    ;; (display " ")(display fn2)
    ;; (display " ")(display fn1)
    ;; (display " ")(display idx)
    ;; (display " ")(display n)
    ;; (newline)
    (cond ((< n 3) n)
          ((< idx n)
            (cond ((< idx 2) (f-iter 0 1 2 (+ idx 1) n))
                  (else
                   (f-iter fn2 fn1 (+ fn1 (* 2 fn2) (* 3 fn3)) (+ idx 1) n)
                  )
            )
          )
          (else fn1)
    )
)


;; test
(display "---------------- rescu -----------------") (newline)
(display (fa 0)) (newline)
(display (fa 1)) (newline)
(display (fa 2)) (newline)
(display (fa 3)) (newline)
(display (fa 4)) (newline)
(display (fa 5)) (newline)
(display (fa 6)) (newline)


(display "---------------- iter -----------------") (newline)

(display (fb 0)) (newline)
(display (fb 1)) (newline)
(display (fb 2)) (newline)
(display (fb 3)) (newline)
(display (fb 4)) (newline)
(display (fb 5)) (newline)
(display (fb 6)) (newline)

(display "---------------- iter vs rescu -----------------") (newline)
(display (fb 100)) (newline)
(display (fb 25)) (newline)
(display (fa 25)) (newline)

