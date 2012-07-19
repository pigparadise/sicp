(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (count-leaves t)
  (accumulate + 0
              (map (lambda (item)
                     (cond ((null? item) 0)
                           ((not (pair? item)) 1)
                           (else (count-leaves item))
                     )
                   )
                   t
              )
  )
)


(define x (cons (list 1 2) (list 3 4 (list 5))))
(newline)
(display x)
(newline)
(display (count-leaves x))
(newline)
(display (count-leaves (list x x)))
(newline)