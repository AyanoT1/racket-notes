#lang play

; parse e intepr estaban definidas pero me da paja pasarlas en limpio...
(deftype Value
  (num n)
  (bool b)
  (pair l r)
  (id x)
  (add l r)
  (sub l r)
  (with x e b)
  )

; subst id expr expr -> expr
; sustituye el identificador id por una expresion en otra expresion
(define (subst x val e)
  (match e
    [(num n) e]
    [(id y) (if (eq? x y) val e)]
    [(add l r) (add (subst x val l) (subst x val r))]
    [(sub l r) (sub (subst x val l) (subst x val r))]
    [(with y n_expr w_body) (with y (subst x val n_expr) (if (eq? x y) w_body (subst x val w_body)))]
    ))

; cuidadihno con el problema del scope
; el programa: {with {x 10} {with {x 20} {+ x x}}} tiene que dar 40
; pero si reemplazamos todos los x por 10 a lo weon nos echamos la wea
; el programa: {with {x 10} {with {x x} {+ x x}}} tiene que dar 20
; no hay que tocarle el cuerpo al with pero hay que evaluar el named expression
; aweonao

(test (subst 'x (num 10) (add (id 'x) (id 'x))) (add (num 10) (num 10)))

; en la clase hay varios ejemplos más de testing con los casos más borde
; pero tampoco los voy a poner aca porque ando con tendinitis :'v

; Hay 3 tipos de identificadores: libres, enlazados y de definicion.
; Substituir debe reemplazar todas las ocurrencias libres de un identificador.
; en {with {x e} b}, el scope de x es b, excluyendo los scopes anidados del mismo identificador
; esto se llama alcance léxico/estático
; (alcance = región de texto)

; Resumen final de la clase: problema general de la sustitución, sus tipos (lazy, eager),
; casos problematicos y definición del scope.