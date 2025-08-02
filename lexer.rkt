#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-empty-tokens tokens/punctuation 
  (LEFT_PAREN
    RIGHT_PAREN
    COMMA
    EOF))

(define-empty-tokens
  tokens/operator 
    (ASSIGN
    PLUS
    MINUS
    ASTERISK
    SLASH
    CARET
    TILDE
    BANG
    QUESTION
    COLON))

(define-tokens 
  tokens/value 
  (NAME))


(define pratt/lexer
  (lexer
    ;; Punctuation
   ["(" (token-LEFT_PAREN)]
   [")" (token-RIGHT_PAREN)]
   ["," (token-COMMA)]

   ;; Operators
   ["=" (token-ASSIGN)]
   ["+" (token-PLUS)]
   ["-" (token-MINUS)]
   ["*" (token-ASTERISK)]
   ["/" (token-SLASH)]
   ["^" (token-CARET)]
   ["~" (token-TILDE)]
   ["!" (token-BANG)]
   ["?" (token-QUESTION)]
   [":" (token-COLON)]

   ;; Values

   ;; Matches a letter followed by zero or more letters/numbers.
   [(:: alphabetic (:* (:or alphabetic numeric)))
   (token-NAME (string->symbol lexeme))]

    ;; Misc

   ;; Skip any whitespace
   [(union #\space #\newline #\tab #\return) (pratt/lexer input-port)]

   ;; Rule for the end of the input
   [(eof) token-EOF]))

(define (lex-all in)
  (let loop ([token (pratt/lexer in)]
             [result empty])
    (if (equal? token-EOF token)
      (reverse result)
      (loop (pratt/lexer in)
            (cons token result)))))

(call-with-input-string "(Test) * (A + B)" lex-all)

