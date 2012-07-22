(define nil ())

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq))
)

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval i n))
   )

   (enumerate-interval 1 n)
  )
)

(define (unique-triples n)
  (flatmap
   (lambda (pair)
     (map (lambda (j) (append pair (list j)))
          (enumerate-interval (cadr pair) n))
   )
   (unique-pairs n)
  )
)

(define (make-triple-sum t)
  (list (car t) (cadr t) (cadr (cdr t))
        (+ (car t) (cadr t) (cadr (cdr t)))
  )
)

(define (sum-triples n s)
  (define (sum-filter? t)
    (<= (+ (car t) (cadr t) (cadr (cdr t))) s)
  )
  (map make-triple-sum (filter sum-filter? (unique-triples n)))
)

(newline)
(display (unique-triples 6))
(newline)(newline)
(display (sum-triples 6 9))