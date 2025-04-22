#lang play

#|

Resumen de las clases online

Identificadores:
- Substitución

Funciones:
- Substitución
- Ambientes

Alcance estático vs dinámico

! Alcance Dinámico no es inutil, se usa para implementar excepciones !
pero es un mal comportamiento por defecto

|#

; Interprete de la clase pasada

(print-only-errors)

; LAMBDAS !
;---------;

#|

  Funciones de primer orden vs orden superior

  Primer orden: Val->Val, Bool->Bool, Val->Bool, etc.
  Orden superior:
    map :: (a -> b) x a[] -> b[]
    addn :: Num -> (Num -> Num)
  _______________________________________
  Funciones de Primera Clase: Fun ⊆ Val

|#
; EXTENSIÓN DEL LENGUAJE CON FUNCOINES DE ORDEN SUPERIOR

{fun {x} x}
{fun {x} {+ x 1}}
{with {id {fun {x} x}} {id 10}}

{fun {n}
     {fun {m}
          {+ n m}}}

#|
<expr> ::= <num>
         | {+ <expr> <expr>}
         | {- <expr> <expr>}
         | {if0 <expr> <expr> <expr>}
         | {with {<sym> <expr>} <expr>}
         | <id>
         - {<sym> <expr>} !!! esto se rompe
         + {<expr> <expr>} -> porque algunas expresiones son funciones
         + {fun {<sym>} <expr>} -> nuevo nodo para soportar funciones
|#
(deftype _Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with id named-expr body)
  (id s)
  (app f-name arg-expr))

;; parse :: s-expr -> Expr
#| where
   <s-expr> ::= <num>
              | <sym>
              | (list '+ <s-expr> <s-expr>)
              | (list '- <s-expr> <s-expr>)
              | (list 'if0 <s-expr> <s-expr> <s-expr>)
              | (list 'with (list <sym> <s-expr>) <s-expr>)
              | (list <sym> <s-expr>)
|#
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c)
                            (parse t)
                            (parse f))]
    [(list 'with (list x e) b)
     (with x (parse e) (parse b))]
    [(list f a) (app f (parse a))]))

;; function definition
(deftype _FunDef
  (fundef name arg body))

;; lookup-fundef :: sym Listof(FunDef) -> FunDef
(define (lookup-fundef f funs)
  (match funs
    ['() (error 'lookup-fundef "function not found: ~a" f)]
    [(cons (and fd (fundef fn _ _)) rest)
     (if (symbol=? fn f)
         fd
         (lookup-fundef f rest))]))

;; ambiente (Environment)
; ADT (Abstract Data Type)
; empty-env : Env
; extend-env : Sym Val Env -> Env
; lookup-env : Sym Env -> Val

(deftype _Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))
(define (extend-env x v e) (aEnv x v e))
(define (lookup-env x e)
  (match e
    [(mtEnv) (error "free identifier: " x)]
    [(aEnv y v next)
     (if (equal? x y)
         v
         (lookup-env x next))]))

; pequeño test suite de Env
(test/exn (lookup-env 'x empty-env)
          "free identifier")
(test  (lookup-env 'x (extend-env 'x 10 empty-env))
       10)

(test (lookup-env 'x (extend-env 'x 20 (extend-env 'x 10 empty-env)))
      20)

(test (lookup-env 'x (extend-env 'y 20 (extend-env 'x 10 empty-env)))
      10)


;; interp :: Expr Env Listof(FunDef) -> number
(define (interp expr env funs)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp l env funs) (interp r env funs))]
    [(sub l r) (- (interp l env funs) (interp r env funs))]
    [(if0 c t f)
     (if (zero? (interp c env funs))
         (interp t env funs)
         (interp f env funs))]
    [(with bound-id named-expr bound-body)
     (interp bound-body       ; extendemos el ambiente
             (extend-env bound-id (interp named-expr env funs) env)
             ;  'n                    5              empty-env
             funs)]
    [(id x) (lookup-env x env)] ; buscamos en el ambiente
    [(app f arg-expr)
     (def (fundef _ farg fbody) (lookup-fundef f funs))
     (interp fbody                 ; extendemos el ambiente <<-- CUAL??
             (extend-env farg (interp arg-expr env funs) empty-env)
             ; usar empty-env: LEXICAL SCOPE
             ; usar env: DYNAMIC SCOPE
             funs)]))

;; run :: s-expr [listof(FunDef)] -> number
(define (run prog [funs '()])
  (interp (parse prog) empty-env funs)) ; empezamos con ambiente vacío

;; tests
(test
 (run '{double {double 5}}
      (list (fundef 'double 'x (parse '{+ x x}))))
 20)

(test/exn (run '{f 10}
               (list (fundef 'f 'n (parse '{n n}))))
          "not found")

(test
 (run '{f 5}
      (list (fundef 'f 'n (parse '{g {+ n 5}}))
            (fundef 'g 'm (parse '{- m 1}))))
 9)

(test (run '{with {x 1}
                  {+ {with {x 2}
                           x}
                     x}})
      3)

; PB: primero tuvimos ALCANCE DINAMICO de los identificadores

; este test permite saber si el lenguaje tiene ALCANCE LEXICO o DINAMICO:

(test/exn (run '{with {n 5}   ; [n->5]
                      {f 2}}
               (list (fundef 'f 'x (parse '{+ x n}))))  ; [x->2] o [x->2,n->5]
          "free identifier")

; en el interprete con subst (ALCANCE LEXICO/ESTATICO), falla con free id n
