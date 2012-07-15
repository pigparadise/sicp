(define (make-point x y)
  (cons x y)
)

(define (x-point p)
  (car p)
)

(define (y-point p)
  (cdr p)
)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
)


;; method a:
;; (define (make-rect x1 y1 x2 y2)
;;   (cons (make-point x1 y1) (make-point x2 y2))
;; )

;; (define (width rect)
;;   (abs (- (x-point (car rect))
;;           (x-point (cdr rect) )
;;        )
;;   )
;; )

;; (define (height rect)
;;   (abs (- (y-point (car rect))
;;           (y-point (cdr rect) )
;;        )
;;   )
;; )

;; method b:
(define (make-rect x y w h)
  (cons (make-point x y) (cons w h))
)

(define (width rect)
  (car (cdr rect))
)

(define (height rect)
  (cdr (cdr rect))
)


;; common api
(define (area rect)
   (* (width rect) (height rect))
)

(define (grith rect)
  (* 2 (+ (width rect) (height rect)))
)

(define (display-rect rect)
  (newline)
  (display "area: ")(display (area rect))
  (newline)
  (display "grith: ")(display (grith rect))
)


;; (display-rect  (make-rect 1 2 3 4)) ;; method a
(display-rect (make-rect 1 2 2 2)) ;; method b