#lang sweet-exp racket/base

provide class instance define:

require kw-make-struct
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
         (struct name (id ...))
         (define (id d) (name-id d))
         ...)]
    [(class constraint-name:id (~datum =>) name:id (~datum where)
       id:id ...)
     #:with [name-id ...]
     (for/list ([id (in-list (syntax->list #'[id ...]))])
       (format-id #'name "~a-~a" #'name id #:source #'id))
     #'(begin
         (struct name (id ...))
         (define (id d) (name-id d))
         ...)]
    ))

(define-syntax instance
  (syntax-parser
    [(instance (class-name:id instance-name:id) (~datum where)
       [id:id (~datum =) impl:expr]
       ...)
     #:with [[kw/impl ...] ...]
     (for/list ([id (in-list (syntax->list #'[id ...]))]
                [impl (in-list (syntax->list #'[impl ...]))])
       (list (datum->syntax id (~> id syntax-e symbol->string string->keyword) id id)
             impl))
     #'(define instance-name (make/kw class-name kw/impl ... ...))]
    ))

(define-syntax define:
  (syntax-parser
    [(define: ([constraint-class:id instance:id] ...)
       id:id
       val:expr)
     #'(define (id instance ...)
         val)]))

