#lang play

#|
s-expr ::= <num>
        | <sym>
        | {+ <s-expr> <s-expr>}
        | {- <s-expr> <s-expr>}
        | {if0 <s-expr> <s-expr> <s-expr>}
        | {with {<sym> <s-expr>} <s-expr>}

|#

;; free-vars :: s-expr -> number
(define (free-vars expr)
  (free-vars-aux expr '())
  )

(define (free-vars-aux expr idl)
  (match expr
    [(? number?) 0]
    [(list '+ br1 br2) (+ (free-vars-aux br1 idl) (free-vars-aux br2 idl))]
    [(list '- br1 br2) (+ (free-vars-aux br1 idl) (free-vars-aux br2 idl))]
    [(list 'if0 ifcond br1 br2) (+ (free-vars-aux br1 idl) (free-vars-aux br2 idl) (free-vars-aux ifcond idl))]
    [(list 'with (list id val) body) (+ (free-vars-aux body (list id idl)) (free-vars-aux val idl))]
    [(? symbol? id) (id (member idl id) 0 1)]
    ))

(test (free-vars '{with {x 2} Racket
                        2 {with {y 6}
                                3 {+ x y}
                                4 }
                        5 }) 0)

(test (free-vars '{with {x 2}
                        8 {+ x y}
                        9 }) 1)