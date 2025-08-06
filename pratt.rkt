#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;; Simple arithmetic expression language to explore Pratt parsing

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
  (PLUS
   MINUS
   ASTERISK
   SLASH
   CARET
   QUESTION
   COLON))

(define-tokens
  token/value
  (NUMBER))

(define pratt/lexer
  (lexer
   ;; Punctuation
   ["(" (token-LEFT_PAREN)]
   [")" (token-RIGHT_PAREN)]
   ["," (token-COMMA)]
   ;; Operators
   ["+" (token-PLUS)]
   ["-" (token-MINUS)]
   ["*" (token-ASTERISK)]
   ["/" (token-SLASH)]
   ["^" (token-CARET)]
   ["?" (token-QUESTION)]
   [":" (token-COLON)]
   ;; Values
   [(:: (:+ numeric)
        (:? (:: "." (:+ numeric))))
    (token-NUMBER (string->number lexeme))]
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
;;   [nud (or (Cursor -> Integer) #f)]
;;   [led (or (Cursor -> Integer) #f)]))
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
         [nud (TokenInfo-nud token-info)]
         [left (begin
                 (unless nud (error 'unexpected-token
                                    "~a"
                                    current-token))
                 (nud (begin
                        (consume! cursor)
                        cursor)))])
    (let loop ([left left])
      (define next-token (peek cursor))
      (define next-token-info (hash-ref token-type->token-info next-token #f))
      (begin
        (unless next-token-info (error 'unexpected-token
                                       "~a"
                                       next-token))
        (if (and next-token-info (> (TokenInfo-lbp next-token-info) rbp))
            (begin
              (consume! cursor)
              (loop ((TokenInfo-led next-token-info) cursor left)))
            left)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nud handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cursor -> Integer
;; interp: Just unwraps the value from the number token
(define (nud/number cursor)
  (token-value (list-ref (Cursor-tokens cursor) (- (Cursor-pos cursor) 1))))

;; Cursor -> Integer
;; interp: if a minus token appears in a prefix position, it should negate
;; the following expression
(define (nud/minus cursor)
  (- (parse-expression cursor 100))) ;; Highest rbp

;; Cursor -> Integer
;; interp: if + appears in the infix position, it should add the
;; immediate left and right expressions
(define (nud/left-paren cursor)
  (let ([result (parse-expression cursor 0)])
    (begin
      (unless (equal? (token-type (peek cursor)) 'RIGHT_PAREN)
        (error "Unmatched ("))
      (consume! cursor)
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; led handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (Integer Natural Cursor -> Integer) ->
;; (Cursor Integer -> Integer)
;; interp: Generic function to build led-handlers
(define (construct-led op)
  (λ (cursor left)
    (let* ([op-type (token-type (list-ref (Cursor-tokens cursor)
                                          (- (Cursor-pos cursor) 1)))]
           [info (hash-ref token-type->token-info op-type #f)]
           [_ (unless info (error "This should never happen"))]
           [bp (TokenInfo-lbp info)])
      (op cursor left bp))))

(define led/question
  (construct-led (λ (cursor left _bp)
                   (let ([next (parse-expression cursor 0)])
                     (if (zero? left)
                         next
                         (begin
                           (unless (equal? (token-type (peek cursor)) 'COLON)
                             (error "Malformed ternery"))
                           (consume! cursor)
                           (parse-expression cursor 0)))))))

(define led/plus
  (construct-led (λ (cursor left bp)
                   (+ left (parse-expression cursor bp)))))

(define led/minus
  (construct-led (λ (cursor left bp)
                   (- left (parse-expression cursor bp)))))

(define led/mul
  (construct-led (λ (cursor left bp)
                   (* left (parse-expression cursor bp)))))

(define led/div
  (construct-led (λ (cursor left bp)
                   (/ left (parse-expression cursor bp)))))

(define led/expo
  (construct-led (λ (cursor left bp)
                   ;; We subtract 1 from the bp to achieve right associativity
                   (expt left (parse-expression cursor (sub1 bp))))))

;; End of led handlers

;; TokenType Natural (or (Cursor -> Integer) #f) (or (Cursor -> Integer) #f)
;; EFFECT: Adds a token type to token info entry for the given token type
(define (define-token-info type lbp [nud #f] [led #f])
  (hash-set! token-type->token-info type (TokenInfo lbp nud led)))

(define-token-info 'NUMBER 0 nud/number)
(define-token-info 'LEFT_PAREN 0 nud/left-paren)
(define-token-info 'RIGHT_PAREN 0)
(define-token-info 'COLON 0)
(define-token-info 'EOF 0)

(define-token-info 'QUESTION 10 #f led/question)
(define-token-info 'PLUS 10 #f led/plus)
(define-token-info 'MINUS 10 nud/minus led/minus)
(define-token-info 'ASTERISK 20 #f led/mul)
(define-token-info 'SLASH 20 #f led/div)
(define-token-info 'CARET 30 #f led/expo)

;; Port -> (Listof Token)
;; interp: Lexer; produces token by reading the input port
(define (pratt/lex in)
  (let loop ([token (pratt/lexer in)]
             [result empty])
    (if (equal? 'EOF token)
        (reverse (cons token result))
        (loop (pratt/lexer in)
              (cons token result)))))

;; String -> integer
;; interp: Entrypoint; Parses (and interprets) the given arithmetic expr
(define (pratt/parse str)
  (let ([tokens (call-with-input-string str pratt/lex)])
    (parse-expression (Cursor tokens 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit
           rackunit/text-ui)
  ;; Helper to run the lexer on a string for testing
  (define (lex-string str)
    (call-with-input-string str pratt/lex))

  (define pratt-tests
    (test-suite "Pratt Parser Tests"
                (test-suite
                 "Lexer"
                 (test-case "Tokenizes numbers"
                            (check-equal?
                             (lex-string "123 45")
                             (list (token-NUMBER 123)
                                   (token-NUMBER 45)
                                   (token-EOF))))

                 (test-case "Tokenizes operators"
                            (check-equal?
                             (lex-string "+ - * / ^ ? :")
                             (list (token-PLUS)
                                   (token-MINUS)
                                   (token-ASTERISK)
                                   (token-SLASH)
                                   (token-CARET)
                                   (token-QUESTION)
                                   (token-COLON)
                                   (token-EOF))))

                 (test-case "Tokenizes punctuation"
                            (check-equal? (lex-string "( ) ,")
                                          (list
                                           (token-LEFT_PAREN)
                                           (token-RIGHT_PAREN)
                                           (token-COMMA)
                                           (token-EOF))))

                 (test-case "Ignores whitespace"
                            (check-equal? (lex-string "  1 \n+\t2  ")
                                          (list
                                           (token-NUMBER 1)
                                           (token-PLUS)
                                           (token-NUMBER 2)
                                           (token-EOF))))

                 (test-case "Handles empty string"
                            (check-equal?
                             (lex-string "") (list (token-EOF))))

                 (test-case "Complex expression tokenization"
                            (check-equal? (lex-string "(5 + 1) * 2")
                                          (list (token-LEFT_PAREN)
                                                (token-NUMBER 5)
                                                (token-PLUS)
                                                (token-NUMBER 1)
                                                (token-RIGHT_PAREN)
                                                (token-ASTERISK)
                                                (token-NUMBER 2)
                                                (token-EOF)))))

                (test-suite
                 "Parser"
                 (test-case "Parses a single number"
                            (check-equal? (pratt/parse "123") 123))

                 (test-case "Simple addition"
                            (check-equal? (pratt/parse "5 + 8") 13))

                 (test-case "Simple subtraction"
                            (check-equal? (pratt/parse "10 - 4") 6))

                 (test-case "Simple multiplication"
                            (check-equal? (pratt/parse "3 * 7") 21))

                 (test-case "Simple division"
                            (check-equal? (pratt/parse "20 / 5") 4))

                 (test-case "Honors operator precedence (PEMDAS)"
                            (check-equal?
                             (pratt/parse "2 + 3 * 4") 14)
                            (check-equal?
                             (pratt/parse "2 * 3 + 4") 10))

                 (test-case "Honors parentheses"
                            (check-equal?
                             (pratt/parse "(2 + 3) * 4") 20))

                 (test-case "Handles left-associativity for +-*/"
                            (check-equal?
                             (pratt/parse "10 - 5 - 2") 3)
                            (check-equal?
                             (pratt/parse "16 / 4 / 2") 2))

                 (test-case
                  "Handles right-associativity for exponentiation"
                  (check-equal?
                   ; 2^(3^2) = 2^9 = 512
                   (pratt/parse "2 ^ 3 ^ 2") 512)
                  (check-not-equal?
                   ; (2^3)^2 = 8^2 = 64
                   (pratt/parse "2 ^ 3 ^ 2") 64))

                 (test-case "Handles unary minus (prefix)"
                            (check-equal? (pratt/parse "-10") -10)
                            (check-equal? (pratt/parse "5 + -2") 3)
                            (check-equal?
                             (pratt/parse "5 * -2") -10))

                 (test-case "Handles complex nested expressions"
                            (check-equal?
                             (pratt/parse
                              "- (2 + 3) * (10 / (2*2) + -1)")
                             (-
                              (* (+ 2 3)
                                 (+ (/ 10 (* 2 2)) -1)))))

                 (test-suite
                  "Ternary operator"
                  (test-case "Selects 'else' branch when condition is non-zero"
                             (check-equal? (pratt/parse "1 ? 10 : 20") 20)
                             (check-equal? (pratt/parse "-5 ? 10 : 20") 20))

                  (test-case "Selects 'if' branch when condition is zero"
                             (check-equal? (pratt/parse "0 ? 10 : 20") 10))

                  (test-case "Ternary precedence is low"
                             ;; (1+1) -> 2 -> 'else' branch -> 20
                             (check-equal? (pratt/parse "1 + 1 ? 10 : 20") 20)
                             ;; (5*2) -> 10 -> 'else' branch -> (20+2) = 22
                             (check-equal?
                              (pratt/parse "5 * 2 ? 10 + 1 : 20 + 2") 22)))

                 (test-suite
                  "Error Handling"
                  (test-case "Unmatched parenthesis"
                             (check-exn (regexp "Unmatched \\(")
                                        (λ () (pratt/parse "(1 + 2"))))

                  (test-case "Unexpected token (dangling operator)"
                             (check-exn (regexp "unexpected-token")
                                        (λ () (pratt/parse "1 +"))))

                  (test-case "Malformed ternary (note typo in original code)"
                             (check-exn (regexp "Malformed ternery")
                                        (λ () (pratt/parse "1 ? 2"))))))))
  (run-tests pratt-tests))
