#lang sweet-exp racket/base

provide class instance define:

require kw-make-struct
        "multiple-inheritance-struct.rkt"
        "multiple-inheritance-struct-make.rkt"
        for-syntax racket/base
                   point-free
                   racket/syntax
                   syntax/parse

(define-syntax class
  (syntax-parser
    [(class name:id (~datum where)
       id:id ...)
     #:with [name-id ...]
     (for/list ([id (in-list (syntax->list #'[id ...]))])
       (format-id #'name "~a-~a" #'name id #:source #'id))
     #'(begin
         (define-struct name (id ...))
         (define (id d) (name-id d))
         ...)]
    [(class constraint-name:id ... (~datum =>) name:id (~datum where)
       id:id ...)
     #:with [name-id ...]
     (for/list ([id (in-list (syntax->list #'[id ...]))])
       (format-id #'name "~a-~a" #'name id #:source #'id))
     #'(begin
         (define-struct name #:extends [constraint-name ...] (id ...))
         (define (id d) (name-id d))
         ...)]
    ))

(define-syntax instance
  (syntax-parser
    [(instance (class-name:id instance-name:id) (~datum where)
       (~seq #:super [super-class-name super-instance-name])
       ...
       [id:id (~datum =) impl:expr]
       ...)
     #'(define instance-name (make class-name [#:super super-class-name super-instance-name] ... [id impl] ...))]
    ))

(define-syntax define:
  (syntax-parser
    [(define: ([constraint-class:id instance:id] ...)
       id:id
       val:expr)
     #'(define (id instance ...)
         val)]))

