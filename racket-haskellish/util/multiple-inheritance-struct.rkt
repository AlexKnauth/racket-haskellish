#lang sweet-exp racket/base

provide define-struct
        for-syntax struct-id
                   struct-info-constructor-id
                   struct-info-supers
                   struct-info-fields

require racket/bool
        racket/unsafe/ops
        syntax/parse/define
        for-syntax racket/base
                   racket/list
                   racket/syntax
                   syntax/parse

module+ test
  require rackunit


begin-for-syntax
  struct struct-info (name constructor-id prop-id supers fields) ; opaque
  ;; a Struct-Info is
  ;; (make-struct-info #:name Symbol
  ;;                   #:constructor-id Identifier
  ;;                   #:prop-id Identifier
  ;;                   #:supers (Listof Struct-Info)
  ;;                   #:fields (Listof (Cons Symbol Identifier)))
  ;; supers is an ordered list containing the struct-infos of the super-structs
  ;; fields is an ordered association list mapping the field names to the accessors, in order
  define (make-struct-info #:name name
                           #:constructor-id constructor-id
                           #:prop-id prop-id
                           #:supers supers
                           #:fields fields)
    struct-info name constructor-id prop-id supers fields
  define-syntax-class struct-id
    #:attributes (info)
    pattern s:id
      #:attr info
      syntax-local-value #'s (lambda () #f)
      #:when (struct-info? (attribute info))
  define-syntax-class private-struct-id
    #:attributes (info prop-id)
    pattern :struct-id
      #:with prop-id:id
      syntax-property
        struct-info-prop-id (attribute info)
        'disappeared-use
        list
          syntax-local-introduce #'s


define-syntax define-struct
  syntax-parser
    group
      define-struct s:id (field:id ...)
      syntax
        define-struct s #:extends [] (field ...)
    group
      define-struct s:id #:extends [super:private-struct-id ...] (field:id ...)
      #:with [s-field ...]
      for/list ([field (in-list (syntax->list #'[field ...]))])
        format-id #'s "~a-~a" #'s field #:source field #:props #'s
      #:with make-s
      format-id #'s "make-~a" #'s #:source #'s #:props #'s
      #:with s?
      format-id #'s "~a?" #'s #:source #'s #:props #'s
      #:with prop-s
      format-id #'s "prop:~a" #'s #:source #'s #:props #'s
      #:do
        define super-n (length (syntax->list #'[super ...]))
        define field-n (length (syntax->list #'[field ...]))
      #:with [super-i:nat ...]
      range super-n
      #:with [field-i:nat ...]
      for/list ([i (in-range field-n)])
        {i + super-n}
      syntax
        begin
          define-values [prop-s s? get-s-prop]
            make-struct-type-property 's #f
              list
                cons super.prop-id
                  lambda (x) 'super-i
                ...
          struct s-struct (super ... field ...)
            #:property prop-s
            #f
          define (make-s super ... field ...)
            s-struct super ... field ...
          define-accessor s-field get-s-prop field-i
          ...
          define-syntax s
            make-struct-info #:name 's
                             #:constructor-id #'make-s
                             #:prop-id #'prop-s
                             #:supers list
                                        (syntax-local-value #'super)
                                        ...
                             #:fields list
                                        cons 'field #'s-field
                                        ...


define-simple-macro
  define-accessor s-field:id get-s-prop:id i:nat
  define (s-field s-instance)
    define prop (get-s-prop s-instance)
    cond
      (false? prop)
        unsafe-struct-ref s-instance 'i
      else
        s-field
          unsafe-struct-ref s-instance prop


module+ test
  define-struct a (x y z)
  define-struct b (x y z)
  define-struct c #:extends [a b] (x y z)
  test-case "a"
    define a (make-a 1 2 3)
    check-equal? (a? a) #true
    check-equal? (a-x a) 1
    check-equal? (a-y a) 2
    check-equal? (a-z a) 3
  test-case "b"
    define b (make-b 1 2 3)
    check-equal? (b? b) #true
    check-equal? (b-x b) 1
    check-equal? (b-y b) 2
    check-equal? (b-z b) 3
  test-case "c"
    define a (make-a 1 2 3)
    define b (make-b 4 5 6)
    define c (make-c a b 7 8 9)
    check-equal? (c? c) #true
    check-equal? (c-x c) 7
    check-equal? (c-y c) 8
    check-equal? (c-z c) 9
    test-case "c inheriting from a"
      check-equal? (a? c) #true
      check-equal? (a-x c) 1
      check-equal? (a-y c) 2
      check-equal? (a-z c) 3
    test-case "c inheriting from b"
      check-equal? (b? c) #true
      check-equal? (b-x c) 4
      check-equal? (b-y c) 5
      check-equal? (b-z c) 6

