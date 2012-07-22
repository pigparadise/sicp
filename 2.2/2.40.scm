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



(define (prime-sum? pair)
  (define (prime? n)
    (= n (find-divisor n 2))
  )
  
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((= 0 (remainder n test-divisor)) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))
    )
  )

  (prime? (+ (car pair) (cadr pair)))
)

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval i n)) ; just change here
   )

   (enumerate-interval 1 n)
  )
)

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n)))
)


(newline)
(display (unique-pairs 6))(newline)
(display (prime-sum-pairs 6))(newline)