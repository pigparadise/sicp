;; prove
;; x = (a, b)
;; y = (c, d)

;; x * y
;; => (lowerx * lowery, upperx * uppery)
;; => (centerx * (1 - px) * centery * (1 - py),
;;     centerx * (1 + px) * centery * (1 + py),
;;    )

;; let A (centerx * centery)
;; => (A*(1 - px) * (1 - py), A*(1 + px) * (1 + py))
;; => (A*(1 - px*py - px - py), A*(1 + px*py + px + py))

;; because px and py is very small, py * px -> 0
;; => (A*(1 - (px + py)), A*(1 + px + py))

;; new center: (centerx * centery)
;; new precent: (px + py)