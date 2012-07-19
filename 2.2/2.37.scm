(define nil ())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate
             op
             init
             (map (lambda (seq) (car seq)) seqs) ;; get all car
            ) 
       
            (accumulate-n 
             op
             init 
             (map (lambda (seq) (cdr seq)) seqs) ;; get all cdr
            )
      )
  )
)

(define (dot-product v w)
  (accumulate + 0 (map * v w))
)

(define (matrix-*-vector m v)
  (map (lambda(w) (dot-product w v)) m)
)

(define (transpose mat)
  (accumulate-n cons nil mat)
)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v))
         m
    )
  )
)

(define v (list 1 2 3 4))
(define m (list (list 1 2 3 4) 
                (list 4 5 6 6) 
                (list 6 7 8 9)
          )
)

(newline)
(display m)
(newline)

(display (dot-product v v))
(newline)

(display (matrix-*-vector m v))
(newline)

(display (transpose m))
(newline)

(display (matrix-*-matrix m (transpose m)))
