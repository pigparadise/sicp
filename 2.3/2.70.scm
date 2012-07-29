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


(define (in-seq x seq)
  (cond ((null? seq) false)
        ((eq? x (car seq)) true)
        (else (in-seq x (cdr seq)))
  )
)

(define (encode-symbol symbol tree)
  (cond ((null? tree) '())
        ((leaf? tree) 
         (if (eq? symbol (symbol-leaf tree))
             '()
             (error "unknown symbol")
         )
        )
        ((in-seq symbol (symbols (left-branch tree)))
         (append '(0) (encode-symbol symbol (left-branch tree)))
        )
        ((in-seq symbol (symbols (right-branch tree)))
         (append '(1) (encode-symbol symbol (right-branch tree)))
        )
        (else (error "unknown symbol"))
  )
)
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree)
      )
  )
)

;;test
(define pairs (list (make-leaf 'A 4)
                    (make-leaf 'NA 16)
                    (make-leaf 'BOOM 1)
                    (make-leaf 'SHA 3)
                    (make-leaf 'GET 2)
                    (make-leaf 'YIP 9)
                    (make-leaf 'JOB 2)
                    (make-leaf 'WAH 1)
              )
)

(newline)
(define msg '(Get a job
              Sha na na na na na na na na
              Get a job
              Sha na na na na na na na na
              Wah yip yip yip yip yip yip yip yip yip
              Sha boom
             )
)

(define tree (generate-huffman-tree pairs))
(display tree)(newline)
(display (encode '(na) tree))(newline)
(display (encode '(get) tree))(newline)
(define result (encode msg tree))
(display result)(display (length result))(newline)

