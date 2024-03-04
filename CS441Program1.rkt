#lang racket

;; Define a custom data type for tokens using define-struct
(define-struct ID (value))
(define-struct NUM (value))
(define-struct PLUS ())
(define-struct MINUS ())
(define-struct MULTIPLY ())
(define-struct DIVIDE ())
(define-struct LESS_THAN ())
(define-struct GREATER_THAN ())
(define-struct LESS_THAN_OR_EQUAL ())
(define-struct GREATER_THAN_OR_EQUAL ())
(define-struct NOT_EQUAL ())
(define-struct EQUAL ())
(define-struct TRUE ())
(define-struct FALSE ())
(define-struct LPAREN ())
(define-struct RPAREN ())
(define-struct EOF ())

;; Tokenize a string
(define (tokenize str)
  (cond
    [(regexp-match #rx"[a-zA-Z][a-zA-Z0-9]*" str) (make-ID str)]
    [(regexp-match #rx"[0-9]+" str) (make-NUM str)]
    [(equal? str "+") (make-PLUS)]
    [(equal? str "-") (make-MINUS)]
    [(equal? str "*") (make-MULTIPLY)]
    [(equal? str "/") (make-DIVIDE)]
    [(equal? str "<") (make-LESS_THAN)]
    [(equal? str ">") (make-GREATER_THAN)]
    [(equal? str "<=") (make-LESS_THAN_OR_EQUAL)]
    [(equal? str ">=") (make-GREATER_THAN_OR_EQUAL)]
    [(equal? str "<>") (make-NOT_EQUAL)]
    [(equal? str "=") (make-EQUAL)]
    [(equal? str "true") (make-TRUE)]
    [(equal? str "false") (make-FALSE)]
    [(equal? str "(") (make-LPAREN)]
    [(equal? str ")") (make-RPAREN)]
    [(equal? str "$$") (make-EOF)]
    [else (error "Invalid token" str)]))

;; Define parsing functions for each non-terminal in the grammar

(define (parse-program tokens)
  (match tokens
    [(list (list line ...) EOF)
     (parse-linelist line)]
    [else 'Syntax error]))

(define (parse-linelist lines)
  (match lines
    [(list line rest ...)
     (match (parse-line line)
       ['Syntax error 'Syntax error]
       ['Accept (parse-linelist rest)])]
    ['epsilon 'Accept]))

(define (parse-line line)
  (match line
    [(list label stmt tail)
     (match (parse-stmt stmt)
       ['Syntax error 'Syntax error]
       ['Accept (parse-linetail tail)])]
    [else 'Syntax error]))

(define (parse-stmt stmt)
  (match stmt
    [(list "read" (ID id))
     'Accept]
    [(list "write" expr)
     (match (parse-expr expr)
       ['Syntax error 'Syntax error]
       ['Accept 'Accept])]
    [(list "if" LPAREN bool RPAREN stmt)
     (match (parse-boolean bool)
       ['Syntax error 'Syntax error]
       ['Accept (parse-stmt stmt)])]
    [(list "while" LPAREN bool RPAREN linelist "endwhile")
     (match (parse-boolean bool)
       ['Syntax error 'Syntax error]
       ['Accept (parse-linelist linelist)])]
    [(list "goto" (ID id))
     'Accept]
    [(list "gosub" (ID id))
     'Accept]
    [(list "return")
     'Accept]
    [(list "break")
     'Accept]
    [(list "end")
     'Accept]
    [else 'Syntax error]))

(define (parse-linetail tail)
  (match tail
    [(list ";" stmt ...)
     (match (parse-stmt stmt)
       ['Syntax error 'Syntax error]
       ['Accept (parse-linetail stmt)])]
    ['epsilon 'Accept]))

(define (parse-boolean bool)
  (match bool
    [(list "true") 'Accept]
    [(list "false") 'Accept]
    [(list expr op expr)
     (match op
       [(or "<" ">" "<=" ">=" "<>" "=") (match (parse-expr expr)
                                           ['Syntax error 'Syntax error]
                                           ['Accept (match (parse-expr expr)
                                                       ['Syntax error 'Syntax error]
                                                       ['Accept 'Accept])])]
       [else 'Syntax error])]
    [else 'Syntax error]))

(define (parse-expr expr)
  (match expr
    [(list (ID id) etail)
     (match etail
       [(list PLUS next-expr)
        (match (parse-expr next-expr)
          ['Syntax error 'Syntax error]
          ['Accept 'Accept])]
       [(list MINUS next-expr)
        (match (parse-expr next-expr)
          ['Syntax error 'Syntax error]
          ['Accept 'Accept])]
       [(list MULTIPLY next-expr)
        (match (parse-expr next-expr)
          ['Syntax error 'Syntax error]
          ['Accept 'Accept])]
       [(list DIVIDE next-expr)
        (match (parse-expr next-expr)
          ['Syntax error 'Syntax error]
          ['Accept 'Accept])]
       ['epsilon 'Accept])]
    [(list (NUM num) etail)
     (match etail
       [(list PLUS next-expr)
        (match (parse-expr next-expr)
          ['Syntax error 'Syntax error]
          ['Accept 'Accept])]
       [(list MINUS next-expr)
        (match (parse-expr next-expr)
          ['Syntax error 'Syntax error]
          ['Accept 'Accept])]
       [(list MULTIPLY next-expr)
        (match (parse-expr next-expr)
          ['Syntax error 'Syntax error]
          ['Accept 'Accept])]
       [(list DIVIDE next-expr)
        (match (parse-expr next-expr)
          ['Syntax error 'Syntax error]
          ['Accept 'Accept])]
       ['epsilon 'Accept])]
    [(list LPAREN expr RPAREN) (parse-expr expr)]
    [else 'Syntax error]))

(define (detect-mismatched-parentheses str)
  (let loop ([chars (string->list str)]
             [open-parens 0])
    (cond
      [(null? chars) open-parens]
      [(< open-parens 0) -1] ; If we encounter more closing parentheses than opening ones
      [(char=? (car chars) #\() (loop (cdr chars) (+ open-parens 1))]
      [(char=? (car chars) #\)) (loop (cdr chars) (- open-parens 1))]
      [else (loop (cdr chars) open-parens)])))

(define (parse-file filename)
  (with-input-from-file filename
    (lambda ()
      (define tokens (port->lines (current-input-port)))
      (define token-list (map (compose tokenize string-trim) tokens))
      (let ((result (parse-program token-list)))
        (cond
          [(string? result) (printf "Syntax error: ~a\n" result)]
          [(= (detect-mismatched-parentheses (port->string (current-input-port))) 0)
           (printf "Accept\n")]
          [else (printf "Syntax error: Mismatched parentheses\n")])))))



;; Example usage: (parse-file "input.txt")