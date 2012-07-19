(define (subsets s)
  (newline)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
        (display "s:")(display s)(display "rest:")(display rest)(newline)
        (append rest 
                (map (lambda (x) (cons (car s) x)) rest))
      )
  )
)

(newline)
(subsets (list 1 2 3))

;; the nil is (), notice that every subsets contains nil
;; (cons x ()) -> (x)

;; subsets of a list s, can divide to two parts.
;; 1. subsets of (s - first_item), let we call it S'
;; 2. first_item combines with very item of S' (include nil)

