#lang sweet-exp lazy

provide
        (all-defined-out)
        #%module-begin
        #%datum
        #%top
        quote
        provide
        all_defined_out
        all_from_out
        except_out
        require
        only_in
        prefix_in

require
        prefix-in _ lazy
        only-in lazy
          only-in only_in
        only_in lazy
          #%app _app
          begin-for-syntax begin_for_syntax
          define-syntax define_syntax
          all-defined-out all_defined_out
          all-from-out all_from_out
          except-out except_out
          rename-in rename_in
          prefix-in prefix_in
          for-syntax for_syntax
        only_in racket/base
          begin begin_defs
          let rkt_let
        only_in racket/match
          match _match
          match-lambda _match_lambda
          define-match-expander define_match_expander
        only_in syntax/parse/define
          define-simple-macro define_simple_macro
          define-syntax-parser define_syntax_parser
        for_syntax
          rename_in racket/base
            make-rename-transformer make_rename_transformer
          rename_in syntax/parse
            id id_op
            expr expr_op
          "operator-symbol.rkt"

begin_for_syntax
  define-syntax-class op
    pattern id:id_op
      #:when (operator-symbol-id? #'id)
  define-syntax-class id
    pattern (~and id:id_op (~not :op))
  define-syntax-class expr
    pattern (~and expr:expr_op (~not :op))

define_syntax_parser #%app #:datum-literals (-)
  (#%app f:expr_op)
    #'f
  (#%app - a:expr)
    #'(_app _- a)
  (#%app a:expr f:op b:expr)
    #'((f) a b)
  (#%app a:expr f:op)
    #'(_app f a)
  (#%app f:op b:expr)
    #'(λ a -> a f b)
  (#%app f:expr a:expr)
    #'(_app f a)
  (#%app f:expr a:expr b:expr ...+)
    #'(#%app (_app f a) b ...)
  (#%app a:expr ...+ f:op b:expr ...+)
    #'((a ...) f (b ...))
  (#%app a:expr ...+ f:op)
    #'((a ...) f)
  (#%app f:op b:expr ...)
    #'(f (b ...))

;; lambda
define_syntax_parser λ #:datum-literals (->)
  (λ a:id -> body:expr_op ...+)
    #'(_lambda (a) (body ...))
  (λ a:id b:id ...+ -> body:expr_op ...+)
    #'(λ a -> (λ b ... -> body ...))

define_syntax \\ (make_rename_transformer #'λ)

define_syntax def
  lambda (stx)
    syntax-parse stx #:datum-literals (=)
      (def x:id_op = body:expr_op ...+)
        syntax/loc stx (_define x (body ...))
      (def (f) = body:expr_op ...+)
        syntax/loc stx (def f = body ...)
      (def (a:expr f:op b:expr) = body:expr_op ...)
        syntax/loc stx (def ((f) a b) = body ...)
      (def (a:expr f:op) = body:expr_op ...)
        syntax/loc stx (def ((f) a) = body ...)
      (def (f arg:expr ...+) = body:expr_op ...+)
        syntax/loc stx (def f = (λ arg ... -> body ...))
      (def (~and a (~not =)) (~and b (~not =)) ...+ = body:expr_op ...+)
        syntax/loc stx (def (a b ...) = body ...)

begin_for_syntax
  define-syntax-class defs_clause
    pattern (~and cls [a:expr_op ...+ = b:expr_op ...+])
      #:with definition
      syntax/loc #'cls (def . cls)

define_syntax_parser defs #:datum-literals (=)
  (defs cls:defs_clause ...)
    #'(begin_defs cls.definition ...)

define_syntax_parser let #:datum-literals (= in)
  group
    let cls:defs_clause ...
      in b:expr_op ...+
    #'(rkt_let ()
        cls.definition
        ...
        (b ...))

define_syntax_parser if #:datum-literals (then else)
  group
    if a:expr_op ...+
      then b:expr_op ...+
      else c:expr_op ...+
    #'(_app _if (a ...) (b ...) (c ...))
  group
    (if (~and a:expr_op (~not then)) ...+
      then (~and b:expr_op (~not then) (~not else)) ...+
      else (~and c:expr_op (~not else)) ...+)
    #'(_app _if (a ...) (b ...) (c ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

def binary_op f a b = (_app f a b)
def apply_op f lst = (_app _apply f lst)

define_simple_macro
  def_binary_ops
    id1:id_op id2:id
    ...
  defs
    (id1) = binary_op id2
    ...

define_simple_macro
  def_apply_ops
    id1:id_op id2:id
    ...
  defs
    (id1) = apply_op id2
    ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

def True = #t
def False = #f

;; make_generic_range : (a -> a) -> (a -> Bool) -> a -> [a]
def make_generic_range next stop =
  let
    range n0 =
      if stop n0
        then '()
        else (_app _cons n0 (range (next n0)))
    in range

def make_infinite_number_range dn =
  let
    next n = (_app _+ n dn)
    stop n = #f
    in make_generic_range next stop

def make_number_range dn nf =
  let
    next n = (_app _+ n dn)
    stop n = (_app _< nf n)
    in make_generic_range next stop

def make_range n0 n1 nf =
  make_number_range (_app _- n1 n0) nf n0

def make_infinite_range n0 n1 =
  make_infinite_number_range (_app _- n1 n0) n0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

define_syntax_parser #%top-interaction
  (#%top-interaction . expr:expr)
    define expanded (local-expand #'expr 'top-level (list #'#%expression))
    syntax-parse expanded #:literals (#%expression)
      (~or expr:id_op (#%expression expr))
        #'(_#%top-interaction . (_app _identity expr)) ; so that toplevel-forcer is called
      expr
        #'(_#%top-interaction . expr)

begin_for_syntax
  define-syntax-class exprop!\| #:datum-literals (\|)
    pattern (~and :expr_op (~not \|))

define_syntax_parser list_macro #:datum-literals (.. \| <-)
  (list_macro [a:expr (b:expr ..)])
    #'(make_infinite_range a b)
  (list_macro [a:expr (b:expr .. c:expr)])
    #'(make_range a b c)
  (list_macro [(a:expr ..)])
    #'(let [b = _add1 a]
        [in make_infinite_range a b])
  (list_macro [(a:expr .. c:expr)])
    #'(let [b = _add1 a]
        [in make_range a b c])
  (list_macro [(a:exprop!\| ...+ \| x:id <- xs:expr) (y:id <- ys:expr) ... c:expr ...])
    #'(for*/list ([x (_in-list (_!list xs))]
                  [y (_in-list (_!list ys))] ...
                  #:when (_! (_app _and c ...)))
        (a ...))
  (list_macro [a:expr ...])
    #'(_app _list a ...)

define_match_expander tuple_macro
  syntax-parser ; match expander
    (tuple_macro (a:expr))
      #'a
    (tuple_macro (a:expr ...))
      #'(vector a ...)
  syntax-parser ; normal macro
    (tuple_macro (a:expr))
      #'(_app _identity a)
    (tuple_macro (a:expr ...))
      #'(_app _vector-immutable a ...)


