#lang play
(print-only-errors #t)

(deftype BinTree
  [leaf]
  [node val left right])

#;(define (contains? bt val)
    (if ((leaf?) bt) #f
        (or (equal? (node-val bt) val)
            (contains? (node-left bt) val)
            (contains? (node-right bt) val))))


(define (contains? bt val)
  (match bt
    [(leaf) #f]
    [(node v l r) (or (equal? v val)
                      (contains? l val)
                      (contains? r val))]
    ))

(test (contains? (leaf) 6) #f)
(test (contains? (node 3 (leaf) (leaf)) 2) #f)
(test (contains? (node 5
                       (node 4 (leaf) (leaf))
                       (node 6 (node 7 (leaf) (leaf)) (leaf)))
                 6) #t)


#|
<expr> :: (num <number?>)
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
|#

(deftype Expr
  [num n]
  [add l r]
  [sub l r])

(test (Expr? (add (add (num 1) (num 2)) (num 3))) #t)

; calc :: Expr -> number?
; Evalúa una expersión aritmética
(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]))

(test (calc (num 1)) 1)
(test (calc (add (num 1) (num 2))) 3)
(test (calc (sub (num 1) (add (num 2) (num 3)))) -4)
(test (calc (sub (add (num 2) (num 3)) (num 1))) 4)

;; Sintaxis concreta??
; (add (num 1) (num 2)) es sintaxis abstracta
; -> conveniente para procesar, pero no para escribir a mano

; {+ 1 2}
; neceistamos una funcion que vaya de
; sintaxis-concreta -> sintaxis->abstracta (un parser) (absolute cinema)

; parse: Src -> Expr

#|
 pero qué es Src?
 - texto (secuencia de caracteres)

{+ 1 2}
{+      1
    2}
son lo mismo?
depende de lo que uno elija (contrasten \t en Java y Python)


en python 1 + 2 = 1            +    2

- foo(123 + 2798324)
tokenizer / lexer
tokens: "foo" "(" "123" "+" "2798324" ")"   // god

lexer -> parser
|#


#|
s-expr (symbolic expression)

<s-expr> ::= <atom>
          | (<s-expr>*)

<atom> ::= number | string | symbol | ...
|#

#|
<Src> ::= <number?>
       | ('+ <Src> <Src>)
       | ('- <Src> <Src>)
|#


; parse: Src -> Expr
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(list '+ s1 s2) (add (parse s1) (parse s2))]
    [(list '- s1 s2) (sub (parse s1) (parse s2))]
    ))

(test (parse '{+ 1 2}) (add (num 1) (num 2)))
(test (parse 5) (num 5))

(test (calc (parse '{+ 1 2})) 3)
(test (calc (parse '5)) 5)

(define (run prog)
  (calc (parse prog)))

(test (run '{+ 1 2}) 3)
(test (run 5) 5)
(test (run '{+ {- 23 12} {+ 2 {- 5 2}}}) 16)