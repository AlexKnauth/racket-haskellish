#lang sweet-exp racket/base

provide print

require racket/list
        racket/match
        racket/format
        racket/promise
        my-cond/iffy
        only-in racket/base
          print -print

define (print v [out (current-output-port)])
  my-cond
    if (promise? v)
      print (force v) out
    if (number? v)
      -print v out
    else-if (list? v)
      my-cond
        if (null? v)
          write-string "[]" out
        else-if (andmap char? v)
          -print (list->string v) out
        else
          print-list-ish v out print "[" ", " "]"
    else-if (vector? v)
      print-list-ish (vector->list v) out print "(" ", " ")"
    else-if (char? v)
      match (~v (string v))
        "\"'\""
          write-string "'\\''" out
        (regexp #rx"^\"(.*)\"$" (list _ s))
          write-string "'" out
          write-string s out
          write-string "'" out
    else-if (boolean? v)
      my-cond
        if v
          write-string "True" out
        else
          write-string "False" out
    else
      error 'print "not supported for ~v" v
  void()

define (print-list-ish lst out rec open sep close)
  match lst
    '()
      write-string open out
      write-string close out
    (cons fst rst)
      write-string open out
      rec fst out
      for ([x (in-list rst)])
        write-string sep out
        rec x out
      write-string close out

