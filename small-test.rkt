#lang s-exp "small-lang.rkt"


(require (planet dyoo/chaining-compare))

(printf "hi\n")

(chaining-compare 3 < (+ 2 2) string<? 5 < 6 < 7)