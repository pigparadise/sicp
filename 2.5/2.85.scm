(put 'project '(rational)
     (lambda (r)
       (make-scheme-number (floor (/ (numer r) (denom r)))
    )
  )
)

(put 'project '(real)
     (lambda (r)
       (let ((scheme-rat (rationalize (inexact->exact r) 1/100)))
         (make-rational (numerator scheme-rat)
                        (denominator scheme-rat)
         )
       )
     )
)

(put 'project '(complex)
     (lambda (c) (make-real (real-part c)))
)


(define (drop num)
  (let ((project-proc (get 'project (list (type-tag num))))
       )
    (if project-proc
        (let ((dropped (project-proc (contents num)))
             )
          (if (equ? num (raise dropped))
              (drop dropped)
              num
          )
        )
        num
    )
  )
)


(define (apply-generic-r op . args)
  (define (no-method type-tags)
    (error "No method for these types" (list op type-tags))
  )

  (define (raise-into s t)
    "Tries to raise s into the type of t. On success,
    returns the raised s. Otherwise, returns #f"
    (let ((s-type (type-tag s))
          (t-type (type-tag t))
         )
      (cond
        ((equal? s-type t-type) s)
        ((get 'raise (list s-type))
         (raise-into ((get 'raise (list s-type)) (contents s)) t)
        )
        (t #f)
      )
    )
  )

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((o1 (car args))
                    (o2 (cadr args))
                   )
                (cond
                  ((raise-into o1 o2)
                   (apply-generic-r op (raise-into o1 o2) o2)
                  )
                  ((raise-into o2 o1)
                   (apply-generic-r op o2 (raise-into o2 o1))
                  )
                  (t (no-method type-tags))
                )
              )
              (no-method type-tags)
          )
      )
    )
  )
)

;; but i think it can't handler common ancestor