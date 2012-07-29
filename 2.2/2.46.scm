;; 2.46
(define (make-vect x y)
  (list x y)
)

(define (xcort-vect vect)
  (car vect)
)

(define (ycort-vect vect)
  (cadr vect)
)

(define (add-vect v1 v2)
  (make-vect (+ (xcort-vect v1) (xcort-vect v2))
             (+ (ycort-vect v1) (ycort-vect v2))
  )
)

(define (sub-vect v1 v2)
  (make-vect (- (xcort-vect v1) (xcort-vect v2))
             (- (ycort-vect v1) (ycort-vect v2))
  )
)

(define (scale-vect s v)
  (make-vect (* s (xcort-vect v))
             (* s (ycort-vect v))
  )
)

