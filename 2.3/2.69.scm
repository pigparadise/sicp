(define (make-leaf symbol weight)
  (list 'leaf symbol weight)
)

(define (leaf? object)
  (eq? (car object) 'leaf)
)

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))
  )
)

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)
  )
)

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)
  )
)

(define (add-pair x pairs)
  (cond ((null? pairs) (list x))
        ((> (weight x) (weight (car pairs)))
         (cons (car pairs) (add-pair x (cdr pairs)))
        )
        (else (cons x pairs))
  )
)

(define (make-leaf-set pairs)
  (define (sort pairs result)
    (cond ((null? pairs) result)
          (else 
           (sort (cdr pairs) (add-pair (car pairs) result))
          )
    )
  )
  (sort pairs '())
)

(define (successive-merge pairs)
  (display "merge: ")(display pairs)(newline)(newline)
  (let ((n (length pairs))
       )
    (cond ((= n 0) '())
          ((= n 1) (car pairs))
          (else (successive-merge
                 (add-pair (make-code-tree (car pairs)(cadr pairs))
                           (cddr pairs)
                 )
                )
          )
    )
  )
)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs))
)

;;test
(define pairs (list (make-leaf 'A 4)
                    (make-leaf 'B 3) 
                    (make-leaf 'C 1)
                    (make-leaf 'D 6)
                    (make-leaf 'E 2)
                    (make-leaf 'F 8)
              )
)

(newline)
(display (generate-huffman-tree pairs))(newline)