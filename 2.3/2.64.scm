(define (make-tree root left right)
  (list root left right)
)

(define (root tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))


(define (list->tree items)
  (car (p-tree items (length items)))
)

(define (p-tree items n)
  (if (= n 0)
      (cons '() items)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (p-tree items left-size)))
          (let ((left-tree (car left-result))
                (non-left-items (cdr left-result))
                (right-size (- n (+ left-size 1)))
               )
            (let ((this-root (car non-left-items))
                  (right-result (p-tree (cdr non-left-items) right-size))
                 )
              (let ((right-tree (car right-result))
                    (remaining-items (cdr right-result))
                   )
                (cons (make-tree this-root left-tree right-tree)
                      remaining-items
                )
              )
            )
          )
        )
      )
  )
)

(newline)

(display (list->tree '(1 3 5 7 9 11)))


;;   5
;;  /   \
;; 1     9
;;  \   / \
;;   3 7  11

;; (p-tree items n) make first n element of items into left tree

;; it use the (n + 1) / 2th( nearly middle ) element as the root
;; then make the rest of items into right tree
;; n will divide by 2 very time, it makes the tree banance
;; 

;; every time will find a root, cost O(N)