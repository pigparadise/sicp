(define (make-monitored func)
  (define ncalls 0)
  (define (reset-count) (set! ncalls 0))
  (define (dispatch . args)
    (cond ((equal? args '(how-many-calls)) ncalls)
          ((equal? args '(reset-count)) ncalls (reset-count))
          (else
           (set! ncalls (+ ncalls 1))
           (apply func args)
          )
    )
  )
  dispatch
)

;; test
(newline)
(define s (make-monitored sqrt))
(display (s 100)) (newline)
(display (s 13)) (newline)
(display (s 'how-many-calls)) (newline)
(display (s 'reset-count)) (newline)
(display (s 'how-many-calls)) (newline)