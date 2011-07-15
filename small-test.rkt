#lang s-exp "small-lang.rkt"


(require (rename-in (planet dyoo/chaining-compare) (chaining-compare compare)))

(printf "hi\n")

(compare (+ 1 1) < (+ 2 2) <= 5 < (* 2 3) < 7)

(printf "---\n")

;; We want the error messages to be very specific
#;(compare)
#;compare

(compare 3 < 3 < 4)