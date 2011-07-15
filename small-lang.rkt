#lang racket


(provide (except-out (all-from-out racket)
                     #%app)
         (rename-out [my-app #%app]))
                        

(define-syntax (my-app stx)
  (syntax-case stx ()
    [(_ op rand ...)
     (syntax/loc stx
       (begin (#%app printf "applying ~e\n" (#%app list (#%app object-name op) rand ...))
              (#%app op rand ...)))]))