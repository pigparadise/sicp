(define (variable? x) (symbol? x))
(define (same-variable? a b)
  (and (variable? a) (variable? b) (eq? a b))
)

(define (=number? exp num)
  (and (number? exp) (= exp num))
)

(define (has seq x)
  (cond ((null? seq) false)
        ((eq? (car seq) x) true)
        (else (has (cdr seq) x))
  )
)

(define (make-sum . items)
  (let ((items (filter (lambda (x) (not (=number? x 0))) items))
       )
    (cond ((null? items) 0)
          ((= 1 (length items)) (car items))
          (else (append (list '+) items))
    )
  )
)

(define (make-product . items)
  (cond ((has items 0) 0)
        (else 
         (let ((items (filter (lambda (x) (not (=number? x 1))) items)))

           (cond ((null? items) 0)
                 ((= 1 (length items)) (car items))
                 (else (append (list '*) items))
           )
         )
        )
  )
)

(define (make-exp base n)
 (cond ((=number? base 0) 0)
        ((=number? n 0) 1)
        ((=number? base 1) 1)
        (else (list '** base n))
  )
)

(define (sum? x)
  (and (pair? x) (eq? (car x) '+ ))
)
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*))
)
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))


(define (exponentitation? x)
  (and (pair? x) (eq? (car x) '**))
)
(define (base p) (cadr p))
(define (exponent p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)

        ((variable? exp)
         (if (same-variable? exp var) 1 0)
        )

        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)
         )
        )

        ((product? exp)
         (make-sum 
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var)
          )
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)
          )
         )
        )

        ((exponentitation? exp)
         (make-product
          (exponent exp)

          (make-product
           (make-exp (base exp) (- (exponent exp) 1))
           (deriv (base exp) var)
          )

         )
        )
        
        (else (error "unknow expression type -- DERiV" exp))
  )
)

(newline)
(display (make-sum 1 2 3 0 1))
(newline)

(display (make-sum 'a 'b 'c))
(newline)

(display (make-product 'a 'b 'c))
(newline)

(display (make-product 'a 0 1))
(newline)

(display (make-product 'a 1 'b))
(newline)

(display (deriv '(* x y (+ x 3)) 'x))
(newline)


