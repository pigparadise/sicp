(define (make-tree root left right)
  (list root left right)
)

(define (root tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))


(define (tree->list-1 tree)
  (cond ((null? tree) '())
        ((number? tree) (list tree))
        (else (append (tree->list-1 (left-branch tree))
                      (cons (root tree)
                            (tree->list-1 (right-branch tree))
                      )
              )
        )
  )
)
 
(define (tree->list-2 tree)
  (define (copy-to-list tree result)
    (cond ((null? tree) result)
          ((number? tree) (cons tree result))
          (else 
           (copy-to-list (left-branch tree)
                         (cons (root tree)
                               (copy-to-list (right-branch tree) result)
                         )
           )
          )
    )
  )

  (copy-to-list tree '())
)

(newline)
(define a (make-tree 7 (make-tree 3 1 5) (make-tree 9 '() 11)))
(display a)
(newline)
(display (tree->list-1 a))
(newline)

(display (tree->list-2 a))
(newline)


(define b (make-tree 3
                     1
                     (make-tree 7 
                                5 
                                (make-tree 9 '() 11)
                     )
          )
)
(display b)
(newline)
(display (tree->list-1 b))
(newline)

(display (tree->list-2 b))
(newline)

(define c (make-tree 5 (make-tree 3 1 '()) (make-tree 9 7 11)))
(display c)
(newline)
(display (tree->list-1 c))
(newline)

(display (tree->list-2 c))
(newline)


;; a) same
;; b) method 1 is O(N), method 2 is O(N), same