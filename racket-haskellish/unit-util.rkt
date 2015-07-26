#lang racket/base

(provide define-thrushed-unit/infer)

(require racket/unit
         syntax/parse/define
         (for-syntax racket/base
                     racket/match
                     syntax/parse
                     ))

(define-syntax-parser define-thrushed-unit/infer
  [(define-thrushed-unit/infer unit@:id sig^:id u:id)
   #'(define-compound-unit/infer unit@
       (import)
       (export sig^)
       (link u))]
  [(define-thrushed-unit/infer unit@:id sig^:id u1:id u2:id ...+)
   #:with [U1 U2 ...] (generate-temporaries #'[u1 u2 ...])
   #:do [(match-define (list -U1.. ... -U-last) (syntax->list #'(U1 U2 ...)))]
   #:with U-last -U-last
   #:with [U1.. ...] -U1..
   #'(define-compound-unit/infer unit@
       (import)
       (export U-last)
       (link [((U1 : sig^)) u1]
             [((U2 : sig^)) u2 U1..]
             ...))]
  )

