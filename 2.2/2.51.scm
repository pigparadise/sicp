;; a
(define (below-a painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 0.0 1.0)
                              split-point
                              ))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 0.0 1.0)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))



;; filp-horiz -> besize-> rotate270(counterclockwise, or clockwise rotate90)
(define (below-b painter1 painter2)
  (roate270 (beside (flip-horiz painter1) (flip-horiz painter2))))
)
