#lang play

; Ejercicio propuesto: Implementar map con fold

; Observación: list (1 2 3 4 5 6) = cons(1 cons(2 cons(3 cons(4 cons (5 cons(6 cons '()))))))

; Firma de map :: ((A->B) x List A) -> List B
; Firma de fold :: (A x B -> B) x List A -> B

; myMap :: ((A->B) x List A) -> List B
(define (myMap f l)
  (foldr (λ (x acc) (cons (f x) acc)) '() l ))

(myMap add1 (list 1 2 3 4 5))
(map add1 (list 1 2 3 4 5))

; foldr empieza desde el final y termina al incio
; cons va construyendo la lista enlazada aplicando la funcion que entrega el myMap