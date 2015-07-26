#lang racket/base

(provide (all-defined-out))

(define ascii-symbol-chars "!#$%&*+./<=>?@\\^|-~:")
(define ascii-symbol-char-list (string->list ascii-symbol-chars))
(define (operator-symbol-char? c)
  (and (char? c) (member c ascii-symbol-char-list) #t))

(define (operator-symbol-string? str)
  (for/and ([c (in-string str)])
    (operator-symbol-char? c)))

(define (operator-symbol? sym)
  (operator-symbol-string? (symbol->string sym)))

(define (operator-symbol-id? id)
  (and (syntax-property id operator-symbol-property) #t))

(define operator-symbol-property
  'operator-symbol-property)

(define (op-sym-id id)
  (syntax-property id operator-symbol-property #t))

