#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(provide all-defined-out)

;; Token is oneof:
;; - token/punctuation (Symbol)
;; - token/operator (Symbol)
;; - token/value (struct lex/Token ([name : Symbol] [value : Any]))

(define-empty-tokens
  token/punctuation
  (LEFT_PAREN
   RIGHT_PAREN
   COMMA
   EOF))

(define-empty-tokens
  token/operator
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
  token/value
  (NAME
   NUMBER))


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
   [(:: alphabetic (:* (:or alphabetic numeric)))
    (token-NAME (string->symbol lexeme))]
   [numeric (token-NUMBER (string->number lexeme))]
   ;; Misc
   [(union #\space #\newline #\tab #\return) (pratt/lexer input-port)]
   [(eof) token-EOF]))

;; Port -> (Listof Token)
(define (lex in)
  (let loop ([token (pratt/lexer in)]
             [result empty])
    (if (equal? token-EOF token)
        (reverse result)
        (loop (pratt/lexer in)
              (cons token result)))))

;; TODO: TESTS
