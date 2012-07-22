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

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))


(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))


(newline)
(display (enumerate-interval 2 7))(newline)
(display (prime-sum-pairs 6))(newline)
(display (permutations (list 1 2 3)))(newline)
