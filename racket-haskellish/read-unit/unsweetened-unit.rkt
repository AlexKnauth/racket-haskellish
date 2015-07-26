#lang racket/unit

(require racket/match
         racket/port
         syntax/readerr
         syntax/parse
         sweet-exp/read-sig
         sweet-exp/util
         "../operator-symbol.rkt"
         )

(import (rename read^
                [-read read]
                [-read-syntax read-syntax]))
(export read^)

(define (read in)
  (parameterize ([current-readtable (make-haskellish-readtable)])
    (-read in)))

(define (read-syntax src in)
  (parameterize ([current-readtable (make-haskellish-readtable)])
    (-read-syntax src in)))

(define (make-haskellish-readtable [rt (current-readtable)])
  (define rt2
    (make-readtable rt
      #\[ 'terminating-macro list-proc
      #\( 'terminating-macro paren-proc
      #\" 'terminating-macro str-proc
      #\λ 'non-terminating-macro λ-proc
      #\' 'non-terminating-macro quote-proc
      #\` 'terminating-macro backtick-proc
      #\, 'terminating-macro unexpected-comma-proc
      #\⫶ #\| #f
      ))
  (define rt3
    (for/fold ([rt rt2]) ([sym-char (in-string ascii-symbol-chars)])
      (make-readtable rt
        sym-char 'terminating-macro symbol-char-proc)))
  (define rt4
    (for/fold ([rt rt3]) ([num-char (in-string "0123456789")])
      (make-readtable rt
        num-char 'non-terminating-macro num-proc)))
  rt4)

(define (list-proc c in src ln col pos)
  (port-count-lines! in)
  (define-values [_1 _2 pos1] (port-next-location in))
  (define rev
    (let loop ([rev '()])
      (define c2 (peek-char in))
      (cond [(eof-object? c2)
             (raise-read-eof-error "expected `]` to close `[`" src ln col pos 1)]
            [(char=? c2 #\])
             (read-char in)
             rev]
            [(char-whitespace? c2)
             (read-char in)
             (loop rev)]
            [else
             (define stx (read-element src in c #\] #\, ln col pos))
             (syntax-parse stx
               [()
                (define-values [src ln col pos span]
                  (values (syntax-source stx) (syntax-line stx) (syntax-column stx)
                          (syntax-position stx) (syntax-span stx)))
                (raise-read-error "expected something" src ln col pos span)]
               [_
                (let loop2 ()
                  (define c3 (peek-char in))
                  (cond [(eof-object? c3) (void)]
                        [(char=? c3 #\,) (read-char in) (void)]
                        [(char-whitespace? c3) (read-char in) (loop2)]
                        [else (void)]))
                (loop (cons stx rev))])])))
  (define-values [_3 _4 pos2] (port-next-location in))
  (datum->syntax #f `(list_macro ,(reverse rev)) (list src ln col pos (+ pos2 (- pos1) 1)) orig-stx))

(define (read-element src in begin-char close-char comma-char ln col pos)
    (define-values [ln1 col1 pos1] (port-next-location in))
    (define rev
      (let loop ([rev '()])
        (define c2 (peek-char in))
        (cond [(eof-object? c2)
               (raise-read-eof-error (format "expected `~a` to close `~a`" close-char begin-char)
                                     src ln col pos 1)]
              [(char=? c2 comma-char) rev]
              [(char=? c2 close-char) rev]
              [(char-whitespace? c2)
               (read-char in)
               (loop rev)]
              [else
               (define stx (read-syntax/recursive src in))
               (loop (cons stx rev))])))
    (define-values [ln2 col2 pos2] (port-next-location in))
    (datum->syntax #f (reverse rev) (list src ln1 col1 pos1 (- pos2 pos1)) orig-stx))

(define (paren-proc c in src ln col pos)
  (port-count-lines! in)
  (define-values [_1 _2 pos1] (port-next-location in))
  (define rev
    (let loop ([rev '()])
      (define c2 (peek-char in))
      (cond [(eof-object? c2)
             (raise-read-eof-error "expected `)` to close `(`" src ln col pos 1)]
            [(char=? c2 #\))
             (read-char in)
             rev]
            [(char-whitespace? c2)
             (read-char in)
             (loop rev)]
            [else
             (define stx (read-element src in c #\) #\, ln col pos))
             (syntax-parse stx
               [()
                (define-values [src ln col pos span]
                  (values (syntax-source stx) (syntax-line stx) (syntax-column stx)
                          (syntax-position stx) (syntax-span stx)))
                (raise-read-error "expected something" src ln col pos span)]
               [_
                (let loop2 ()
                  (define c3 (peek-char in))
                  (cond [(eof-object? c3) (void)]
                        [(char=? c3 #\,) (read-char in) (void)]
                        [(char-whitespace? c3) (read-char in) (loop2)]
                        [else (void)]))
                (loop (cons stx rev))])])))
  (define-values [_3 _4 pos2] (port-next-location in))
  (define datum
    (match rev
      [(list a) a]
      [rev `(tuple_macro ,(reverse rev))]))
  (datum->syntax #f datum (list src ln col pos (+ pos2 (- pos1) 1)) orig-stx))

(define (str-proc c in src ln col pos)
  (define orig-str-rt
    (make-readtable (current-readtable)
      #\" #\" #f))
  (port-count-lines! in)
  (define-values [_1 _2 pos1] (port-next-location in))
  (define stx
    (read-syntax/recursive src in #\" orig-str-rt))
  (define-values [_3 _4 pos2] (port-next-location in))
  (define str (syntax-e stx))
  (define lst (string->list str))
  (datum->syntax #f `(list_macro ,lst) (list src ln col pos (+ pos2 (- pos1) 1)) orig-stx))

(define (quote-proc c in src ln col pos)
  (define rt (make-readtable (current-readtable) c #\" #f))
  (define stx
    (parameterize ([current-readtable rt])
      (read-syntax/recursive src in c rt)))
  (define str (syntax-e stx))
  (unless (= (string-length str) 1)
    (raise-read-error "expected 1 character" src ln col pos (syntax-span stx)))
  (datum->syntax #f (string-ref str 0) (list str ln col pos (syntax-span stx)) orig-stx))

(define (λ-proc c in src ln col pos)
  (datum->syntax #f (string->symbol (string c)) (list src ln col pos 1) orig-stx))

(define (symbol-char-proc c in src ln col pos)
  (define rev-more-symbol-chars
    (let loop ([rev '()])
      (define c2 (peek-char in))
      (cond [(operator-symbol-char? c2)
             (read-char in)
             (loop (cons c2 rev))]
            [else rev])))
  (define str (list->string (cons c (reverse rev-more-symbol-chars))))
  (define sym (string->symbol str))
  (op-sym-id
   (datum->syntax #f sym (list src ln col pos (string-length str)) orig-stx)))

(define (backtick-proc c in src ln col pos)
  (port-count-lines! in)
  (define id
    (read-syntax/recursive src in #f))
  (define span (syntax-span id))
  (unless (identifier? id)
    (raise-read-error (format "expected identifier after backtick character (~a)" c)
                      src ln col pos (or (and span (+ span 1)) 2)))
  (define-values [ln2 col2 pos2] (port-next-location in))
  (define c2 (read-char in))
  (unless (char=? c2 c)
    (raise-read-error (format "expected backtick character (~a)" c)
                      src ln2 col2 pos2 1))
  (op-sym-id id))

(define (unexpected-comma-proc c in src ln col pos)
  (raise-read-error "unexpected comma" src ln col pos 1))

(define (num-proc c in src ln col pos)
  (define rev-more-num-chars
    (let loop ([rev '()])
      (define c2 (peek-char in))
      (cond [(eof-object? c2) rev]
            [(char-numeric? c2)
             (read-char in)
             (loop (cons c2 rev))]
            [(char=? c2 #\.)
             (when (member #\. rev)
               (raise-read-error "multiple decimal points in number literal"
                                 src ln col pos (+ (length rev) 2)))
             (define c3 (peek-char in 1))
             (cond [(and (char? c3) (char=? c3 #\.))
                    rev]
                   [else
                    (read-char in)
                    (loop (cons c2 rev))])]
            [else rev])))
  (define str (list->string (cons c (reverse rev-more-num-chars))))
  (define num (string->number str))
  (datum->syntax #f num (list src ln col pos (string-length str)) orig-stx))

