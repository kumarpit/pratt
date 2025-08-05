#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LEXER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   [(eof) (token-EOF)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parser is ([tokens: (Listof Token)] [pos: Integer])
;; interp: Wrapper around a list of tokens
(struct Parser (tokens pos) #:mutable)

;; Parser -> Token
;; interp: Returns the token at the current position in the token list
(define (peek parser) (list-ref (Parser-tokens parser) (Parser-pos parser)))

;; Parser
;; EFFECT: Advances the position tracked in the Parser by 1
(define (advance! parser) (set-Parser-pos! parser (add1 (Parser-pos parser))))

;; TokenType is Symbol

;; Token -> Symbol
;; interp: Given a token, returns the token's type (name) based on what kind
;; of token it is (empty token vs. value holding token)
(define (token-type t)
  (if (symbol? t) t (token-name t)))

;; TokenInfo is ([lbp : Integer]
;; [nud (or (Parser -> Integer) #f)]
;; [led (or (Parser -> Integer) #f)])
;; interp: contains the left-binding power (lbp), null-denotation and
;; left-denotation handler for a Token
;;
(struct TokenInfo (lbp nud led))

;; DispatchTable is a hash table mapping TokenType -> TokenInfo
(define dispatch-table (make-hash))

(define (parse-expression parser [rbp 0])
  (let* ([current-token-type (token-type (peek parser))]
         [token-info (hash-ref dispatch-table current-token-type)]
         [left ((TokenInfo-nud token-info) (begin
                                             (advance! parser)
                                             parser))])
    (let loop ([left left])
      (define next-token-type (peek parser))
      (define next-token-info (hash-ref dispatch-table next-token-type))
      (if (and next-token-info (> (TokenInfo-lbp next-token-info) rbp))
          (begin
            (advance! parser)
            (loop ((TokenInfo-led next-token-info) parser left)))
          left))))

(define (nud-number p)
  (token-value (list-ref (Parser-tokens p) (- (Parser-pos p) 1))))

(define (nud-prefix-minus p)
  (- (parse-expression p 70)))

(define (led-mul p left)
  (let* ([op-type (token-type (list-ref (Parser-tokens p)
                                        (- (Parser-pos p) 1)))]
         [info (hash-ref dispatch-table op-type)]
         [bp (TokenInfo-lbp info)])
    (* left (parse-expression p bp))))

(define (led-add p left)
  (let* ([op-type (token-type (list-ref (Parser-tokens p)
                                        (- (Parser-pos p) 1)))]
         [info (hash-ref dispatch-table op-type)]
         [bp (TokenInfo-lbp info)])
    (+ left (parse-expression p bp))))

(define (define-token-info type lbp [nud #f] [led #f])
  (hash-set! dispatch-table type (TokenInfo lbp nud led)))

(define-token-info 'NUMBER 0 nud-number)
(define-token-info 'EOF 0)

(define-token-info 'PLUS 10 nud-prefix-minus led-add)
(define-token-info 'ASTERISK 20 #f led-mul)

;; Port -> (Listof Token)
(define (pratt/lex in)
  (let loop ([token (pratt/lexer in)]
             [result empty])
    (begin (println token)
           (if (equal? 'EOF token)
               (reverse (cons token result))
               (loop (pratt/lexer in)
                     (cons token result))))))

(define (parse str)
  (let ([tokens (call-with-input-string str pratt/lex)])
    (parse-expression (Parser tokens 0))))