(define (make-mobile left right)
  (list left right)
)

(define (make-branch length structure)
  (list length structure)
)

;; a
(define (left-brance mobile)
  (car mobile)
)

(define (right-brance mobile)
  (car (cdr mobile))
)

(define (brance-length brance)
  (car brance)
)

(define (brance-structure brance)
  (car (cdr brance))
)

;; b
(define (total-weight mobile)
  (cond ((pair? mobile)
         (+ (total-weight (brance-structure (left-brance mobile)))
            (total-weight (brance-structure (right-brance mobile)))
         )
        )
        (else mobile) ;; leaf, weight value
  )
)

;; c
(define (balance? mobile)
  (display "mobile: ")(display mobile)(newline)
  (cond ((pair? mobile)
         (let ((left (left-brance mobile))
               (right (right-brance mobile))
              )

           (define left_mobile (brance-structure left))
           (define right_mobile (brance-structure right))
           (display "left:")(display left)(newline)
           (display "right:")(display right)(newline)
           (and (= (* (total-weight left_mobile) (brance-length left))
                   (* (total-weight right_mobile) (brance-length right))
                )
                (balance? left_mobile)
                (balance? right_mobile)
           )
         )
        )
        (else true); leaf, not mobile
  )
)

;; d: need change right-brance and brance-structure
;; (define (make-mobile left right)
;;   (cons left right)
;; )
;; (define (make-branch length structure)
;;   (cons length structure)
;; )

;; (define (left-brance mobile)
;;   (car mobile)
;; )

;; (define (right-brance mobile)
;;   (cdr mobile)
;; )

;; (define (brance-length brance)
;;   (car brance)
;; )

;; (define (brance-structure brance)
;;   (cdr brance)
;; )

;; test
(newline)
(define a (make-mobile (make-branch 3 4) (make-branch 4 3)))
(define b (make-mobile (make-branch 12 1) (make-branch 2 6)))
(define c (make-mobile (make-branch 2 a) (make-branch 2 b)))

;; weight
(display (total-weight a))(newline)
(display (total-weight b))(newline)
(display (total-weight c))(newline)

;; balance
(display (balance? a))(newline)
(display (balance? b))(newline)
(display (balance? c))(newline)

