#lang sweet-exp racket/base

provide make

require "multiple-inheritance-struct.rkt"
        for-syntax racket/base
                   syntax/id-table
                   syntax/parse
module+ test
  require rackunit


define-syntax make
  lambda (stx)
    syntax-parse stx
      group
        make s:struct-id
          [#:super super:id super-expr:expr]
          ...
          [field:id field-expr:expr]
          ...
        #:do
          define s-info (attribute s.info)
          define s-info-supers (struct-info-supers s-info)
          define s-info-fields (struct-info-fields s-info)
          define s-info-super-symbols (map struct-info-name s-info-supers)
          define s-info-field-symbols (map car s-info-fields)
          define given-supers
            for/hash ([super (in-list (syntax->list #'[super ...]))]
                      [super-expr (in-list (syntax->list #'[super-expr ...]))])
              unless (member (syntax-e super) s-info-super-symbols)
                raise-syntax-error #f
                  format "unexpected super: ~a" (syntax-e super)
                  stx
                  super
              values (syntax-e super) super-expr
          define given-fields
            for/hash ([field (in-list (syntax->list #'[field ...]))]
                      [field-expr (in-list (syntax->list #'[field-expr ...]))])
              unless (member (syntax-e field) s-info-field-symbols)
                raise-syntax-error #f
                  format "unexpected field: ~a" (syntax-e field)
                    stx
                    field
              values (syntax-e field) field-expr
        #:with s-constructor
        syntax-property
          struct-info-constructor-id s-info
          'disappeared-use
          list
            syntax-local-introduce #'s
        #:with [s-super-expr ...]
        for/list ([s-super (in-list s-info-super-symbols)])
          hash-ref given-supers s-super
            lambda ()
              raise-syntax-error #f
                format "missing super ~a" (syntax-e s-super)
                stx
        #:with [s-field-expr ...]
        for/list ([s-field (in-list s-info-field-symbols)])
          hash-ref given-fields s-field
            lambda ()
              raise-syntax-error #f
                format "missing field ~a" s-field
                stx
        syntax
          s-constructor s-super-expr ... s-field-expr ...


module+ test
  define-struct a (x y z)
  define-struct b (x y z)
  define-struct c #:extends [a b] (x y z)
  test-case "a"
    define a-inst (make a [x 1] [y 2] [z 3])
    check-equal? (a? a-inst) #true
    check-equal? (a-x a-inst) 1
    check-equal? (a-y a-inst) 2
    check-equal? (a-z a-inst) 3
  test-case "b"
    define b-inst (make b [x 1] [y 2] [z 3])
    check-equal? (b? b-inst) #true
    check-equal? (b-x b-inst) 1
    check-equal? (b-y b-inst) 2
    check-equal? (b-z b-inst) 3
  test-case "c"
    define a-inst (make a [x 1] [y 2] [z 3])
    define b-inst (make b [x 4] [y 5] [z 6])
    define c-inst (make c [#:super a a-inst] [#:super b b-inst] [x 7] [y 8] [z 9])
    check-equal? (c? c-inst) #true
    check-equal? (c-x c-inst) 7
    check-equal? (c-y c-inst) 8
    check-equal? (c-z c-inst) 9
    test-case "c inheriting from a"
      check-equal? (a? c-inst) #true
      check-equal? (a-x c-inst) 1
      check-equal? (a-y c-inst) 2
      check-equal? (a-z c-inst) 3
    test-case "c inheriting from b"
      check-equal? (b? c-inst) #true
      check-equal? (b-x c-inst) 4
      check-equal? (b-y c-inst) 5
      check-equal? (b-z c-inst) 6

