#lang racket-haskellish/core

;; #lang racket-haskellish
;; a racket language inspired by Haskell
;; (doesn't have static types)

provide
        (all_defined_out)
        except_out (all_from_out ⫶racket-haskellish/core⫶)
          def_binary_ops
          def_apply_ops
          binary_op
          apply_op

require
        prefix_in _ lazy
        only_in lazy
          ⫶#%app⫶ _app
        only_in ⫶racket/match⫶
          match _match

def_binary_ops
  map _map
  filter _filter
  + ⫶_+⫶
  - ⫶_-⫶
  * ⫶_*⫶
  / ⫶_/⫶
  ^ _expt
  div _quotient
  mod _remainder

def_binary_ops
  == ⫶_equal?⫶

def (a /= b) = not (a == b)

def_binary_ops
  < ⫶_<⫶
  > ⫶_>⫶
  <= ⫶_<=⫶
  >= ⫶_>=⫶

def succ = _add1

def_binary_ops
  min _min
  max _max

def even = ⫶_even?⫶
def odd = ⫶_odd?⫶

def_binary_ops
  && _and
  || _or
def not = _not

def_binary_ops
  ++ _append
  : _cons
  !! ⫶_list-ref⫶

def head = _first
def tail = _rest
def last lst =
  _match (⫶_!list⫶ lst)
    (list _ ... last) last
def init lst =
  _match (⫶_!list⫶ lst)
    (list init ... _) init
def length = _length
def null = ⫶_null?⫶
def reverse = _reverse

def take n lst = reverse (revTake n lst)
def revTakeAccum accum n lst =
  if (n == 0) || (null lst)
    then accum
    else revTakeAccum (head lst : accum) (n - 1) (tail lst)
def revTake = revTakeAccum []
def drop n lst =
  if (n == 0) || (null lst)
    then lst
    else drop (n - 1) (tail lst)

def cycle orig_lst =
  let
    cy lst =
      if null lst
        then cy orig_lst
        else (head lst) : (cy (tail lst))
    in cy orig_lst
def repeat x = cycle [x]
def replicate n x =
  let
    accum lst n =
      if n == 0
        then lst
        else accum (x : lst) (n - 1)
    in accum [] n

def_apply_ops
  minimum _min
  maximum _max

def_apply_ops
  sum ⫶_+⫶
  product ⫶_*⫶

def elem x lst =
  if (binary_op _member x lst)
    then True
    else False

def_binary_ops
  . _compose

def (a $ b) = a b

def fst a = (binary_op ⫶_vector-ref⫶ a 0)
def snd a = (binary_op ⫶_vector-ref⫶ a 1)

def pair a b = (a,b)

def zipWith f =
  let
    zipWithF a b =
      if null a || null b
        then []
        else f (head a) (head b) : zipWithF (tail a) (tail b)
    in zipWithF

def zip = zipWith pair

