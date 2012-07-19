;; directly
(define (square-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))
              )
        )
  )
)

;; map
(define (square-tree-bymap tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-bymap sub-tree)
             (square sub-tree)
         )
       )
       tree
  )
)


(define tree (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)
             )
)

(newline)
(display (square-tree tree))(newline)
(display (square-tree-bymap tree))(newline)
