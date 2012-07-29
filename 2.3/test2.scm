(define (make-tree root left right)
  (list root left right)
)

(define (root tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (in-set x set)
  (cond ((null? set) false)
        ((= x (root set)) true)
        ((< x (root set)) (in-set x (left-branch set)))
        ((> x (root set)) (in-set x (right-branch set)))
  )
)

(define (add-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (root set)) set)

        ((< x (root set))
         (make-tree (root set)
                    (add-set x (left-branch set))
                    (right-branch set)
         )
        )

        ((> x (root set))
         (make-tree (root set)
                    (left-branch set)
                    (add-set x (right-branch set))
         )
        )
  )
)

