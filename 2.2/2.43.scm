(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens)
        )
        (queen-cols (- k 1))
   )
 )
 (enumerate-interval 1 board-size)
)


(queen-cols (- k 1) will be repeat board-size times.
total will cost  board-size * board-size * T