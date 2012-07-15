(define (make-interval a b) (cons a b))
(define (lower-bound p) (car p))
(define (upper-bound p) (cdr p))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))
  )
)

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y)))
       )
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
  )
)

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))
                )
  )
)

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)
  )
)


(define (par2 r1 r2)
  (let ((one (make-interval 1 1))
       )
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)
                  )
    )
  )
)

(define (make-center-percent i percent)
  (cons (* i (- 100 percent) 0.01)
        (* i (+ 100 percent) 0.01)
  )
)

(define A (make-center-percent 100 1))
(define B (make-center-percent 100 2))
(newline)(display "A = ")(display A)
(newline)(display "B = ")(display B)

(newline)(display "p1 A/A = ")(display (par1 A A))
(newline)(display "p2 A/A = ")(display (par2 A A))

(newline)(display "p1 A/B = ")(display (par1 A B))
(newline)(display "p2 A/B = ")(display (par2 A B))


;; 2.15
;; 1 / (1/R1 + 1/R2)
;; 1/R1 => R2/(R2*R1), 1/R2 => R1/(R1*R2)
;; this is base on  1 = R1/R1 = R2/R2
;; the result of 2.14 show that par2 A/A is more accurate


;; 2.16 it's hard, at last i find the solution on http://eli.thegreenplace.net/2007/07/27/sicp-section-214/

;; eliben explains:

;; In doing arithmetic, we rely on some laws to hold without giving them much thought. Speaking mathematically, the real numbers are fields. 

;; For example, we expect to have an inverse element for addition – for each element A to have an element A’ so that A + A’ =. It is easy to check that this doesn’t hold for intervals!

;; An inverse element for multiplication also doesn’t exist (this is the problem we saw in exercise 2.14). 

;; The distributive law doesn’t hold – consider the expression [1,2] * ([-3,-2] + [3,4]) – it makes a difference whether you do the additions or the multiplication first.
