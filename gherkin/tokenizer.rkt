#lang racket/base
(provide make-tokenizer)

(require brag/support
         racket/string)

(define-lex-abbrev reserved-terms (:or "Given" "When" "Then" "And" "Examples" "Feature" "Scenario" "Background"))
(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define gherkin-lexer
  (lexer-srcloc
   ["\n" (token 'NEWLINE lexeme)]
   [whitespace (token lexeme #:skip? #t)]
   ["|" (token 'SEPARATOR lexeme)]
   [(from/stop-before reserved-terms "\n")
    (let* ([words       (string-split lexeme)]
           [term        (string->symbol (string-upcase (trim-ends "" (car words) ":")))]
           [description (string-join (cdr words))])
      (token term description))]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:+ (:or alphabetic numeric punctuation symbolic)) (token 'STRING lexeme)]
   #;[(:seq ":" (:+ alphabetic)) (token 'ARGUMENT (string->symbol (substring lexeme 1)))]
   [(:or (from/to "\"" "\"") (from/to "\'" "\'"))
    (token 'STRING
           (substring lexeme 1 (sub1 (string-length lexeme))))]
   [(from/stop-before "#" "\n") (token 'COMMENT lexeme)]))

(define (make-tokenizer ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (define (next-token)
    (gherkin-lexer ip))
  next-token)
