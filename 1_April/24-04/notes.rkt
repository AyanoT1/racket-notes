#lang play

(print-only-errors)

#| agregando funciones de primera clase
ejemplo:

{fun {x} x}

{fun {x} {+ x 1}}

{{fun {x} {+ x 1}} 100}

{with {id {fun {x} x}}
   {id 10}}

{with {id {fun {x} x}}
   {id {id 10}}}

{fun {n}
 {fun {m}
   {+ n m}}}

{{addn 10} 20}

{{if0 cond
      {fun {x} x}
      {fun {x} {+ x 1}}}
  19}
|#

#|
<expr> ::= <num>
         | {+ <expr> <expr>}
         | {- <expr> <expr>}
         | {if0 <expr> <expr> <expr>}
         | {with {<sym> <expr>} <expr>}
         | <id>
         | {<expr> <expr>}      ; aplicacion "generalizada"
         | {fun {<sym>} <expr>} ; funciones anonimas
|#
(deftype Expr
  (num n)  ; num-n
  (add l r) ; add-l add-r
  (sub l r)
  (if0 c t f)
  (with id named-expr body) ; TODO: eliminar
  (id s)
  (app fun-expr arg-expr)
  (fun param body)) ; fun-param fun-body

;; parse :: s-expr -> Expr
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
     (with x (parse e) (parse b))] ;; TODO: transformar a app/fun
    [(list f a) (app (parse f) (parse a))]
    [(list 'fun (list x) e) (fun x (parse e))]))


;; ambiente (Environment)
; ADT (Abstract Data Type)
; empty-env : Env
; extend-env : Sym Val Env -> Env
; lookup-env : Sym Env -> Val

(deftype Env
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

(deftype Val
  (numV n)
  (closureV param body env))

;; interp :: Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(fun x b) (closureV x b env)] ; capturamos el ambiente
    [(add l r) (num+ (interp l env) (interp r env))]
    [(sub l r) (num- (interp l env) (interp r env))]
    [(if0 c t f)
     (if (num-zero? (interp c env))
         (interp t env)
         (interp f env))]
    ; TODO: borrar el caso with
    [(with bound-id named-expr bound-body)
     (interp bound-body
             (extend-env bound-id (interp named-expr env) env))]
    [(id x) (lookup-env x env)]
    [(app fun-expr arg-expr)
     (def (closureV farg fbody fenv) (interp fun-expr env))
     (interp fbody
             (extend-env farg (interp arg-expr env) fenv))]))

(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))

(define (num- n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))

(define (num-zero? n)
  (zero? (numV-n n)))

;; run :: s-expr -> number
(define (run prog)
  (match (interp (parse prog) empty-env)
    [(numV n) n]
    [x x]))

;; tests

(test (run '{{fun {x} {+ x 1}} 100})
      101)

(test
 (run '{with {double {fun {x} {+ x x}}}
             {double {double 5}}})
 20)

(test (run '{with {x 1}
                  {+ {with {x 2}
                           x}
                     x}})
      3)

(test (run '{{if0 20
                  {fun {x} x}
                  {fun {x} {+ x 1}}}
             19})
      20)

(test (run '{with {applyTo10 {fun {f} {f 10}}}
                  {applyTo10 {fun {x} {+ x 10}}}})
      20)


(test (run '{with {addn {fun {n}
                             {fun {m}
                                  {+ n m}}}}
                  {{addn 200} 10}})
      210)

; EJERCICIO: eliminar el with
; with existe en el codigo fuente, pero el parser no debe generar un with
; {with {x 2} {+ x x}}  --> {{fun {x} {+ x x}} 2}

;(interp {fun {m} {+ n m}} [n -> 40])
;(interp {fun {m} {+ n m}} [n -> 2])