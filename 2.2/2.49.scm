; a)
(define (painter-a frame)
  (define x 1.0)
  (define y 1.0)
  (segments->painter 
   (list (make-segment (make-segment 0 0) (make-segment x 0))
         (make-segment (make-segment 0 0) (make-segment 0 y))
         (make-segment (make-segment x 0) (make-segment x y))
         (make-segment (make-segment 0 y) (make-segment x y))
  )
 )
)

; b
(define (painter-a frame)
  (define x 1.0)
  (define y 1.0)
  (segments->painter 
   (list (make-segment (make-segment 0 0) (make-segment x y))
         (make-segment (make-segment x 0) (make-segment 0 y))
  )
 )
)

; c
(define (painter-a frame)
  (define x 1.0)
  (define y 1.0)
  (segments->painter 
   (list (make-segment (make-segment (/ x 2) 0) (make-segment 0 (/ y 2)))
         (make-segment (make-segment (/ x 2) 0) (make-segment x (/ y 2)))
         (make-segment (make-segment (/ x 2) y) (make-segment 0 (/ y 2)))
         (make-segment (make-segment (/ x 2) y) (make-segment x (/ y 2)))
  )
 )
)

; d, i don't want to do it ...