(define (fringe tree)
  (cond ((null? tree) ())
        ((pair? tree)
         (append (fringe (car tree))
                 (fringe (cdr tree))
         )
        )
        (else (list tree))
  )
)

(define x (list (list 1 2) (list 3 4)))
(newline)
(display x)(newline)
(display (fringe x))(newline)
(display (fringe (list x x)))(newline)