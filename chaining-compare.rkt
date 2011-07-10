#lang racket/base

;; Provides convenient syntax for chaining binary boolean comparisons in Racket.
;;
;; For example,
;;
;;   (chaining-compare x <= y < z)
;;
;; will be translated to:
;;
;;   (let ([y-val y])
;;     (if (<= x y-val)
;;        (< y-val z)
;;        #f))
;;
;;

(require (for-syntax racket/base))

(provide chaining-compare)


(define-for-syntax (make-binop-comparison lhs operator rhs src-stx)
 (with-syntax ([lhs lhs]
               [operator operator]
               [rhs rhs])
   (datum->syntax src-stx
                  `(,#'operator ,#'lhs ,#'rhs)
                  (list (syntax-source #'lhs)
                        (syntax-line #'lhs)
                        (syntax-column #'lhs)
                        (syntax-position #'lhs)
                        (if (and (number? (syntax-position #'lhs))
                                 (number? (syntax-position #'rhs))
                                 (number? (syntax-span #'rhs)))

                            (- (+ (syntax-position #'rhs)
                                  (syntax-span #'rhs))
                               (syntax-position #'lhs))
                            #f)))))

(define-syntax (chaining-compare stx)
 (syntax-case stx ()
   [(_ lhs operator rhs)
    (make-binop-comparison #'lhs #'operator #'rhs stx)]

   [(_ lhs operator rhs
       second-operator second-rhs rest-of-chain ...)
    (with-syntax ([rhs-value (datum->syntax #f 'rhs-value
                                            (list (syntax-source #'rhs)
                                                  (syntax-line #'rhs)
                                                  (syntax-column #'rhs)
                                                  (syntax-position #'rhs)
                                                  (syntax-span #'rhs)))])
      (quasisyntax/loc stx
        (let ([rhs-value rhs])
          (if #,(make-binop-comparison #'lhs #'operator #'rhs-value stx)
              (chaining-compare rhs-value second-operator second-rhs
rest-of-chain ...)
              #f))))]

   ;; Error production
   [else
    (raise-syntax-error #f "Expected operands separated by binary operators"
                        stx)]))