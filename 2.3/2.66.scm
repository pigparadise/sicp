(define (make-tree root left right)
  (list root left right)
)

(define (root tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

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

(define (add-dict key value dict)
  (if (null? dict)
      (make-tree (cons key value) '() '())
      (let ((root-node (root dict))
            (left-child (left-branch dict))
            (right-child (right-branch dict))
           )

        (cond
         ((= key (car root-node))
          (make-tree (cons key value)
                     left-child
                     right-child)
         )

         ((< key (car root-node))
          (make-tree root-node
                     (add-dict key value left-child)
                     right-child)
         )

         (else 
          (make-tree root-node
                     left-child 
                     (add-dict key value right-child))
         )
       )
      )
  )
)

(define (lookup key dict)
  (cond ((null? dict) false)
        ((= key (car (root dict)))
         (cdr (root dict)))

        ((< key (car (root dict)))
         (lookup key (left-branch dict)))

        (else 
         (lookup key (right-branch dict))
        )
  )
)

;; test
(newline)
(define d '())
(set! d (add-dict 1 'a d))
(set! d (add-dict 7 'b d))
(set! d (add-dict 3 'c d))
(set! d (add-dict -3 'd d))
(set! d (add-dict 5 'e d))
(set! d (add-dict 0 'f d))
(set! d (add-dict 0 'f d))


(display (lookup 10 d))(newline)
(display (lookup 0 d))(newline)
(display (lookup -3 d))(newline)
