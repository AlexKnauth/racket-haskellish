#lang racket/base

(provide configure)

(require (only-in lazy toplevel-forcer !!)
         "../read-unit/main.rkt"
         )

(define (configure data)
  (toplevel-forcer !!)
  (current-print (make-forcing-printer (current-print)))
  (current-read-interaction haskellish-read-syntax))

(define (make-forcing-printer old-printer)
  (define (forcing-printer v)
    (old-printer ((toplevel-forcer) v)))
  forcing-printer)

