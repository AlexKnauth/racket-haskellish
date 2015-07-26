#lang racket-haskellish

provide chk

require
        only_in rackunit
          ⫶define-binary-check⫶ define_binary_check
          ⫶with-check-info*⫶ with_check_info'
          ⫶make-check-expression⫶ make_check_expression
          ⫶make-check-name⫶ make_check_name
          ⫶make-check-location⫶ make_check_location
        only_in ⫶syntax/parse/define⫶
          ⫶define-syntax-parser⫶ define_syntax_parser
          ⫶~datum⫶
          ⫶define/syntax-parse⫶ with
          ⫶this-syntax⫶ this_syntax
        only_in lazy
          ⫶!!⫶ force'
        only_in ⫶racket/base⫶
          ⫶for-syntax⫶ for_syntax
          ⫶#%app⫶ rkt_app
          list rkt_list
        only_in ⫶racket/function⫶
          thunk
        for_syntax
          only_in ⫶racket/base⫶
            ⫶#%app⫶
            syntax
            ...
          only_in ⫶syntax/srcloc⫶
            ⫶build-source-location-list⫶ syntax_srcloc_list

define_binary_check (checkEqual expr1 expr2)
  force' (expr1 == expr2)

define_syntax_parser chk
  group
    chk a ... (⫶~datum⫶ =) b ...
    with loc_list (syntax_srcloc_list this_syntax)
    syntax
      rkt_app with_check_info'
        rkt_app rkt_list
          rkt_app make_check_name
            quote chk
          rkt_app make_check_expression
            quote
              chk a ... = b ...
          rkt_app make_check_location
            quote loc_list
        thunk
          checkEqual (force' (a ...)) (force' (b ...))

