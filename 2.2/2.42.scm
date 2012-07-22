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


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))

         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens)
                 )
                 (enumerate-interval 1 board-size)
            )
          )
          
          (queen-cols (- k 1))
         )
        )
    )
  )

  (queen-cols board-size)
)

;; empty set of positions, postion: (row, col)
(define empty-board ())

;; pos -> (row, col)
;; safe? -> row_k != row_i and
;;          col_k != col_i and
;;          abs(row_k - col_k) != abs(row_i - col_i)
;; role of col_k: only need check new postions in col_k
(define (safe? col_k positions)
  (define row_k 0)
  (define dist 0)
  (define (iter row_k col_k dist pos_list)
    (cond ((null? pos_list) true)
          (else 
           (let ((row_i (car (car pos_list)))
                 (col_i (cadr (car pos_list))))
             (if (or (= row_k row_i)
                      (= col_k col_i)
                      (= (abs (- row_i col_i)) dist)
                 )
                 false
                 (iter row_k col_k dist (cdr pos_list))
             )
           )
          )
    )
  )

  (cond ((null? positions) true)
        (else (set! row_k (car (car positions)))
              (set! dist (abs (- row_k col_k)))
              (iter row_k col_k dist (cdr positions))
              ;; (null? (filter (lambda (pos)
              ;;                  (let ((row_i (car pos))
              ;;                        (col_i (cadr pos)))
              ;;                    (or (= row_k row_i)
              ;;                        (= col_k col_i)
              ;;                        (= (abs (- row_i col_i)) dist)
              ;;                        )
              ;;                    )
              ;;                )
              ;;                (cdr positions)
              ;;         )
              ;; )
        )
  )
)

;; add (row_i, col_k), we also can filte some pos at here
;; put it not the frist for filter later
(define (adjoin-position new-row k rest-of-queens) 
  (append (list (list new-row k)) rest-of-queens)
)

(define (display-queens n result)
  (define (iter nkind seq)
    (cond ((not (null? seq))
           (display nkind)
           (display ": ")
           (display (car seq))
           (newline)
           (iter (+ nkind 1) (cdr seq))
          )
    )
  )
  (display "queens: ")(display n)
  (display ", total: ")(display (length result))(newline)
  (iter 1 result)
  (newline)
)

(newline)
(map (lambda (i) (display-queens i (queens i)))
     (enumerate-interval 1 11)
)
