#lang s-exp "small-lang.rkt"


(require (planet dyoo/chaining-compare))

(printf "hi\n")

(chaining-compare (+ 1 1) < (+ 2 2) <= 5 < (* 2 3) < 7)
