#lang racket
(require parser-tools/lex)
(require "lexer.rkt")

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
(define DispatchTable (make-hash))

(define (parse-expression parser [rbp 0])
  #f)


