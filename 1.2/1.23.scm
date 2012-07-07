(define (search-for-primes n)
  (cond ((even? n) (search-for-primes (+ n 1)))
        (else (if (prime? n) 
                  n
                  (search-for-primes (+ n 2))
              )
        )
  )
)

(define (prime? n)
  (= n (find-divisor n 2))
)

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((= 0 (remainder n test-divisor)) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))
  )
)

(define (new-prime? n)
  (= n (new-find-divisor n 2))
)

;; (define (next-divisor n)
;;   (if (= n 2) 3 (+ n 2))
;; )
(define (next-divisor n)
  (if (even? n) (+ n 1) (+ n 2))
)

(define (new-find-divisor n test-divisor)
  ;; (display "test-divisor: ")(display test-divisor)(newline)
  (cond ((> (square test-divisor) n) n)
        ((= 0 (remainder n test-divisor)) test-divisor)
        (else (new-find-divisor n (next-divisor test-divisor)))
  )
)


(define (timed-prime-test func n)
  (define begin-time (runtime))
  (func n)
  (- (runtime) begin-time)
)


(define (cmp_time_cost n)
  (display "n: ")(display n)
  (define old (timed-prime-test prime? n))
  (define new (timed-prime-test new-prime? n))
  (display ", old: ")(display old)
  (display ", new: ")(display new)
  (newline)
)
(newline)


(define (test-iter start_num count max-count)
  (define n 1)
  (cond ((< count max-count)
         (set! n (search-for-primes start_num))
         (cmp_time_cost n)
         (test-iter (+ n 1) (+ count 1) max-count)
        )
        (else 1)
  )
)

(test-iter 1e10 0 12)

;; finally i use a func (next-divisor n) and it run about twice as fast!
;; the result on my computer:
;; n: 10000000019., old: .28999999999999204, new: .1599999999999966
;; n: 10000000033., old: .2599999999999909, new: .1599999999999966
;; n: 10000000061., old: .2699999999999818, new: .15000000000000568
;; n: 10000000069., old: .27000000000001023, new: .15000000000000568
;; n: 10000000097., old: .2699999999999818, new: .12999999999999545
;; n: 10000000103., old: .27000000000001023, new: .15000000000000568
;; n: 10000000121., old: .2699999999999818, new: .15000000000000568
;; n: 10000000141., old: .28000000000000114, new: .15000000000000568
;; n: 10000000147., old: .2699999999999818, new: .15000000000000568
;; n: 10000000207., old: .2600000000000193, new: .14999999999997726
;; n: 10000000259., old: .27000000000001023, new: .15000000000000568
;; n: 10000000277., old: .2699999999999818, new: .15000000000000568

