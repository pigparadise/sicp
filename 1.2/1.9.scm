(define (inc x)
  (display "inc ")(display x)(newline)
  (+ x 1)
)

(define (dec x) 
  (display "dec")(display x)(newline)
  (- x 1)
)

;; (define (add a b)
;;   (display a)(display " + ")(display b)(newline)
;;   (if (= a 0)
;;       b
;;       (inc (add (dec a) b))
;;   )
;; )
;; recursive process: inc(inc(inc...(add 0 b)...)))


(define (add a b)
  (display "add ")(display a)(display " ")(display b)(newline)
  (if (= a 0)
      b
      (add (dec a) (inc b))
  )
)
;; iterative process:
;; add 3 6
;; add 2 7
;; add 1 8
;; add 0 9

(add 4 5)