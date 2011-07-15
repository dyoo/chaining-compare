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
  (datum->syntax src-stx
                 `(,operator ,lhs ,rhs)
                 (list (syntax-source lhs)
                       (syntax-line lhs)
                       (syntax-column lhs)
                       (syntax-position lhs)
                       (if (and (number? (syntax-position lhs))
                                (number? (syntax-position rhs))
                                (number? (syntax-span rhs)))
                           
                           (- (+ (syntax-position rhs)
                                 (syntax-span rhs))
                              (syntax-position lhs))
                           #f))))


(define-for-syntax (build-chain stx original-stx)
  (syntax-case stx ()
   [(lhs operator rhs)
    (make-binop-comparison #'lhs #'operator #'rhs stx)]

   [(lhs operator rhs second-operator second-rhs rest-of-chain ...)
    (with-syntax ([rhs-value (datum->syntax #f
                                            (gensym 'rhs-value)
                                            (list (syntax-source #'rhs)
                                                  (syntax-line #'rhs)
                                                  (syntax-column #'rhs)
                                                  (syntax-position #'rhs)
                                                  (syntax-span #'rhs)))])
      (quasisyntax/loc stx
        (let ([rhs-value rhs])
          (if #,(make-binop-comparison #'lhs #'operator #'rhs-value original-stx)
              #,(build-chain #'(rhs-value second-operator second-rhs rest-of-chain ...)
                             original-stx)
              #f))))]
    
   ;; Error production
   [else
    (raise-syntax-error #f "Expected operands separated by binary operators"
                        original-stx)]))


(define-syntax (chaining-compare stx)
  (syntax-case stx ()
    [(_ x ...)
     (build-chain #'(x ...) stx)]))
