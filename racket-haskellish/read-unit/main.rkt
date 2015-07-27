#lang sweet-exp racket/base

provide racket-haskellish-read@
        haskellish-read
        haskellish-read-syntax

require racket/unit
        sweet-exp/read-sig
        only-in racket/base
          read -read
          read-syntax -read-syntax
        sweet-exp/sugar
        "../util/unit-util.rkt"
        "unsweetened-unit.rkt"
module+ test
  require rackunit

define-unit underlying-read@
  (import)
  (export read^)
  define read -read
  define read-syntax -read-syntax

define-thrushed-unit/infer racket-haskellish-read@ read^
  underlying-read@
  sugar@
  unsweetened@

define-values/invoke-unit racket-haskellish-read@
  (import)
  (export (rename read^
                  [haskellish-read read]
                  [haskellish-read-syntax read-syntax]))

module+ test
  define (rd str) (haskellish-read (open-input-string (string-append str "\n")))
  check-equal? (rd "x") 'x
  check-equal? (rd "[x]") '(#%list-macro (x))
  check-equal? (rd "[x,y]") '(#%list-macro (x y))
  check-equal? (rd "f x") '(f x)
  check-equal? (rd "f(x)") '(f (x))
  check-equal? (rd "f(g x)") '(f (g x))
  check-equal? (rd "x'") '|x'|
  check-equal? (rd "x'y") '|x'y|
  check-equal? (rd "a+b") '(a + b)
  check-equal? (rd "a++b") '(a ++ b)
  check-equal? (rd "a.b") '(a |.| b)
  check-equal? (rd "4.0") 4.0
  check-equal? (rd "'a'") #\a
  
