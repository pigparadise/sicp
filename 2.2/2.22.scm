(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items ()))

;; the struct of result is not (cons item1 (cons item2 (cons ...)))
;; is (... (cons (cons item1) item2) ...)
(square-list (list 1 2 3 4))

