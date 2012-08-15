the define of magnitude and apply-generic
(define (magnitude z) (apply-generic 'magnitude z))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))


when use (magnitude z), will call (apply-generic 'magnitude z)

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))

will find '(complex) then use (get op '(complex) find handler -- magnitude
then use function apply to run it
so we need to registers them

apply-generic has been called twice in (magnitude z)
magnitude of complex ->  magnitude of rectangular -> final magnitude