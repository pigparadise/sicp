(define system (make-hash-table))
(define (put key1 key2 value)
  (hash-table-set! system (list key1 key2) value)
)
(define (get key1 key2)
  (hash-table-ref system (list key1 key2))
)

(define (attach-tag type-tag contents)
  (cons type-tag contents)
)
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)
  )
)
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)
  )
)
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (=zero? x)
  (cond ((number? x) (= 0 x))
        (else false)
  )
)

(define (add x y)
  ;; (apply-generic 'add x y)
  (+ x y)
)

(define (mul x y)
  ;; (apply-generic 'mul x y)
  (* x y)
)


(define (install-polynomial-package)
  ;; term
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else 
           (let ((t1 (first-term L1))
                 (t2 (first-term L2))
                )
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 (add-terms (rest-terms L1) L2))
                   )

                   ((< (order t1) (order t2))
                    (adjoin-term t2 (add-terms (rest-terms L2) L1))
                   )
                   
                   (else
                    (adjoin-term
                     (make-term (order t1) (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1) (rest-terms L2))
                    )
                   )
             )
           )
          )
    )
  )

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2)
        )
    )
  )

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2))
           )
           (mul-term-by-all-terms t1 (rest-terms L))
          )
        )
    )
  )

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)
    )
  )

  ;; poly
  (define (make-poly var terms) (cons var terms))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)
                   )
        )
        (error "polys not in same var -- ADD-POLY"
               (list p1 p2)
        )
    )
  )

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)
                   )
        )
        (error "polys not in same var -- MUL-POLY"
               (list p1 p2)
        )
    )
  )

  ;; rest
  (define (tag p) (attach-tag 'polynomial p))
  (define (val p) (contents p))
  (put 'make '(polynomial-term)
       (lambda (order coeff) (make-term order coeff))
  )
  (put 'adjoin '(polynomial-term polynomial-term-list)
       (lambda (t L) (adjoin-term t L))
  )

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms)))
  )
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly (val p1) (val p2))))
  )
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly (val p1) (val p2))))
  )

'done)


(define (make-polynomial-term order coeff)
  ((get 'make '(polynomial-term))
   order coeff)
)
(define (adjoin-polynomial-term term term-list)
  ((get 'adjoin '(polynomial-term polynomial-term-list))
   term term-list)
)
(define (make-polynomial var terms)
  ((get 'make 'polynomial)
   var terms)
)

;; test
(install-polynomial-package)
(define terms_a
  (adjoin-polynomial-term
    (make-polynomial-term 100 1)

    (adjoin-polynomial-term
      (make-polynomial-term 2 2)

      (adjoin-polynomial-term
        (make-polynomial-term 0 1)
        '()
      )
    )
  )
)

(define terms_b
  (list (list 5 1) (list 4 2) (list 2 3) (list 1 -2) (list 0 -5))
)

(define p_a (make-polynomial 'x terms_a))
(define p_b (make-polynomial 'x terms_b))

(newline)
(display "a = ")(display p_a)(newline)
(display "b = ")(display p_b)(newline)

(display "a + b = ")
(display ((get 'add '(polynomial polynomial)) p_a p_b))
(newline)

(display "a * b = ")
(display ((get 'mul '(polynomial polynomial)) p_a p_b))
(newline)
