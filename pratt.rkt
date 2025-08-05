#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LEXER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Token is oneof:
;; - token/punctuation (Symbol)
;; - token/operator (Symbol)
;; - token/value (struct _ ([name : Symbol] [value : Any]))

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
   [(eof) (token-EOF)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cursor is ([tokens: (Listof Token)] [pos: Natual])
;; interp: Wrapper around a list of tokens with helpers to access/modify
;; position
(struct Cursor (tokens pos) #:mutable)

;; Cursor -> Token
;; interp: Returns the token at the current position in the token list
(define (peek cursor) (list-ref (Cursor-tokens cursor) (Cursor-pos cursor)))

;; Cursor
;; EFFECT: Advances the position tracked in the Cursor by 1
(define (consume! cursor) (set-Cursor-pos! cursor (add1 (Cursor-pos cursor))))

;; TokenType is Symbol

;; Token -> TokenType
;; interp: Given a token, returns the token's type (name) based on what kind
;; of token it is (empty token vs. value holding token)
(define (token-type t)
  (if (symbol? t) t (token-name t)))

;; TokenInfo is
;; (struct _ ([lbp : Natural]
;; [nud (or (Cursor -> Integer) #f)]
;; [led (or (Cursor -> Integer) #f)]))
;; interp: contains the left-binding power (lbp), null-denotation and
;; left-denotation handler for a Token
;;
(struct TokenInfo (lbp nud led))

;; token->token-info is a hash table mapping TokenType -> TokenInfo
(define token-type->token-info (make-hash))

;; Cursor Natural -> Integer
;; interp: Main Pratt parser driver loop
(define (parse-expression cursor [rbp 0])
  (let* ([current-token (token-type (peek cursor))]
         [token-info (hash-ref token-type->token-info current-token)]
         [left ((TokenInfo-nud token-info) (begin
                                             (consume! cursor)
                                             cursor))])
    (let loop ([left left])
      (define next-token (peek cursor))
      (define next-token-info (hash-ref token-type->token-info next-token))
      (if (and next-token-info (> (TokenInfo-lbp next-token-info) rbp))
          (begin
            (consume! cursor)
            (loop ((TokenInfo-led next-token-info) cursor left)))
          left))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nud handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cursor -> Integer
;; interp: Just unwraps the value from the number token
(define (nud-number p)
  (token-value (list-ref (Cursor-tokens p) (- (Cursor-pos p) 1))))

;; Cursor -> Integer
;; interp: if a minus token appears in a prefix position, it should negate
;; the following expression
(define (nud-prefix-minus p)
  (- (parse-expression p 70))) ;; Highest rbp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; led handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cursor -> Integer
;; interp: if * appears in the infix position, it should multiply the
;; immediate left and right expressions
(define (led-mul p left)
  (let* ([op-type (token-type (list-ref (Cursor-tokens p)
                                        (- (Cursor-pos p) 1)))]
         [info (hash-ref token-type->token-info op-type)]
         [bp (TokenInfo-lbp info)])
    ;; TODO: Should assert this is indeed a *
    (* left (parse-expression p bp))))

;; Cursor -> Integer
;; interp: if + appears in the infix position, it should add the
;; immediate left and right expressions
(define (led-add p left)
  (let* ([op-type (token-type (list-ref (Cursor-tokens p)
                                        (- (Cursor-pos p) 1)))]
         [info (hash-ref token-type->token-info op-type)]
         [bp (TokenInfo-lbp info)])
    ;; TODO: Should assert this is indeed a +
    (+ left (parse-expression p bp))))

;; End of led handlers

;; TokenType Natural (or (Cursor -> Integer) #f) (or (Cursor -> Integer) #f)
;; EFFECT: Adds a token type to token info entry for the given token type
(define (define-token-info type lbp [nud #f] [led #f])
  (hash-set! token-type->token-info type (TokenInfo lbp nud led)))

(define-token-info 'NUMBER 0 nud-number)
(define-token-info 'EOF 0)

(define-token-info 'PLUS 10 nud-prefix-minus led-add)
(define-token-info 'ASTERISK 20 #f led-mul)

;; Port -> (Listof Token)
;; interp: Lexer; produces token by reading the input port
(define (pratt/lex in)
  (let loop ([token (pratt/lexer in)]
             [result empty])
    (begin (println token)
           (if (equal? 'EOF token)
               (reverse (cons token result))
               (loop (pratt/lexer in)
                     (cons token result))))))

;; String -> integer
;; interp: Entrypoint; Parses (and interprets) the given arithmetic expr
(define (pratt/parse str)
  (let ([tokens (call-with-input-string str pratt/lex)])
    (parse-expression (Cursor tokens 0))))
