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


;; Creates temporaries, and uses the source locations from the srcs.
;; We'll see if this functionality already exists in Racket.
(define-for-syntax (generate-temporaries/locs lst srcs)
  (let ([temps (generate-temporaries lst)])
    (map (lambda (stx src)
           (datum->syntax stx
                          (syntax-e stx)
                          (list (syntax-source src)
                                (syntax-line src)
                                (syntax-column src)
                                (syntax-position src)
                                (syntax-span src))
                          stx
                          stx))
         temps
         (syntax->list srcs))))


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
    (make-binop-comparison #'lhs #'operator #'rhs original-stx)]

   [(lhs operator rhs second-operator second-rhs rest-of-chain ...)
    (with-syntax ([(rhs-value) (generate-temporaries/locs #'(rhs) #'(rhs))])
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
    [(_ lhs y ...)
     (with-syntax ([(lhs-v) (generate-temporaries/locs #'(lhs) #'(lhs))])
       (quasisyntax/loc stx
         (let ([lhs-v lhs])
           #,(build-chain #'(lhs-v y ...) stx))))]))
