;; 2.73-2.74 is not hard, but will costs some time, so i gave up

;; put and get can use a hash table, we can find how to use hash-table
;; in http://srfi.schemers.org/srfi-69/srfi-69.html


(define global-dict (make-hash-table))

(define (put key1 key2 value)
  (hash-table-set! global-dict (list key1 key2) value)
)

(define (get key1 key2)
  (hash-table-ref global-dict (list key1 key2))
)

;; test
(newline)
(put 'a 'b 1)
(put 'a 'b 2)
(put 'a 'c 1)
(put 'op 'type (lambda (x) x))
(display global-dict)(newline)
(display (get 'a 'b))(newline)
(display (get 'a 'c))(newline)
(display ((get 'op 'type) 'x))(newline)
