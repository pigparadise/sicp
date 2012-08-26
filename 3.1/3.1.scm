(define (make-accumulator base)
  (define (dispatch n)
    (set! base (+ base n))
    base
  )
  dispatch
)


;; test
(define A (make-accumulator 5))
(newline)
(display (A 10))(newline)
(display (A 10)) (newline)