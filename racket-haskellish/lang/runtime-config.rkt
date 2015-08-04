#lang racket/base

(provide configure)

(require (only-in lazy toplevel-forcer !!)
         (only-in "../print.rkt" [print haskell-print])
         "../read-unit/main.rkt"
         )

(define (configure data)
  (toplevel-forcer !!)
  (current-print (make-haskell-printer (current-print)))
  (current-read-interaction haskellish-read-syntax))

(define ((make-haskell-printer old-printer) v)
  (define v* ((toplevel-forcer) v))
  (cond [(void? v*) v*]
        [else (old-printer (haskell-printed v))]))

(struct haskell-printed (v)
  #:property prop:custom-write
  (λ (this out mode)
    (haskell-print (haskell-printed-v this) out)))

