#lang s-exp syntax/module-reader
racket-haskellish/racket-haskellish
#:read haskellish-read
#:read-syntax haskellish-read-syntax
#:language-info #(racket-haskellish/lang/language-info get-language-info #f)

(require "../read-unit/main.rkt")
