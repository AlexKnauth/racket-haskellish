#lang sweet-exp racket/base

require math/flonum
        racket/match
        racket/block
        racket/function
        prefix-in rkt: racket/base
        prefix-in rkt: racket/math
        "typeclassish.rkt"
        racket/flonum
        racket/fixnum
module+ test
  require rackunit

class Eq where
  == ; a -> a -> Bool
  /= ; a -> a -> Bool

class Eq => Ord where
  compare ; a -> a -> (U 'LT 'EQ 'GT)

instance (Eq EqNumber) where
  == =
    lambda (a b)
      (rkt:= a b)
  /= =
    lambda (a b)
      (not ((== EqNumber) a b))

instance (Ord OrdNumber) where
  compare =
    lambda (a b)
      cond
        [((== EqNumber) a b) 'EQ]
        [(rkt:< a b) 'LT]
        [else 'GT]

;; f :: Ord a => a a -> a
define: ([Ord a])
  f
  lambda (x y)
    match ((compare a) x y)
      ['LT x]
      ['GT y]
      ['EQ x]

;; f :: OrdImplStruct -> (a a -> a)

;; ((f OrdNumber) 1 2)

;; g :: Any Any -> Any
;; define (g x y)
;;   cond [(and (number? x) (number? y))
;;         ((f EqNumber) x y)]
;;        [else
;;         (error 'bad)]

class Num where
  + ; a -> a -> a
  - ; a -> a -> a
  * ; a -> a -> a
  negate ; a -> a
  abs ; a -> a
  signum ; a -> a
  fromInteger ; Integer -> a

define (fl1/ x)
  (fl/ 1.0 x)

define (flnegate x)
  (fl- 0.0 x)

define (fxnegate x)
  (fx- 0 x)

instance (Num FloatNum) where
  + = fl+
  * = fl*
  - = fl-
  negate = flnegate
  abs = flabs
  signum = flsgn
  fromInteger = fl

instance (Num FixnumNum) where
  + = fx+
  * = fx*
  - = fx-
  negate = fxnegate
  abs = fxabs
  signum = rkt:sgn
  fromInteger = identity

instance (Num NumberNum) where
  + = rkt:+
  - = rkt:-
  * = rkt:*
  negate = rkt:-
  abs = rkt:abs
  signum = rkt:sgn
  fromInteger = identity

class ListLike where
  empty
  cons

instance (ListLike List) where
  empty = '()
  cons = rkt:cons

define-values [Eqq eq?]
  block
    class Eqq where
      eq?
    values Eqq eq?

define EqqAny
  Eqq rkt:eq?

module+ test
  check-equal? ((f OrdNumber) 1 2) 1
  check-equal? ((f OrdNumber) 2 1) 1
  check-equal? ((cons List) 1 ((cons List) "one" (empty List))) '(1 "one")
  check-equal? ((eq? EqqAny) 4 5) #false
  check-equal? ((eq? EqqAny) 4 4) #true



