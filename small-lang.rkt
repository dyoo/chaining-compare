#lang racket


(provide (except-out (all-from-out racket)
                     #%app)
         (rename-out [my-app #%app]))
                        

(define-syntax (my-app stx)
  (syntax-case stx ()
    [(_ op rand ...)
     (quasisyntax/loc stx
       (begin (printf "applying ~s\n" (list (object-name op) rand ...))
              #,(syntax/loc stx
                  (op rand ...))))]))