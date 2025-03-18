#lang play

; Ejercicio propuesto: Implementar Map con Fold
; map :: ((A->B) x List A) -> List B
; fold :: (A x B -> B) x List A -> B

; Ejemplos de map
(map even? (list 1 2 3 4 5))

(map add1 (list 1 2 3 4 5))


; Funciones aditivas
(define (add2 n)
  (+ 2 n))

(map add2 (list 1 2 3 4 5))

(map (λ (n)
       (+ 212 n))
     (list 1 2 3 4 5))

; Funciones que devuelven funciones
; addn :: Num -> (Num -> Num)
(define (addn m)
  (λ (n)
    (+ n m )))

; Uso de addn para devolver una funcion aditiva especifica
(map (addn 67) (list 1 2 3 4 5))
(map (addn 89) (list 1 2 3 4 5))

(define marvel (addn 89))

(marvel 20)

; Ejemplo de diferencias en las firmas de la funcion
; addn :: Num -> (Num -> Num)
; es dinstinto a
;  (Num -> Num) -> Num
; ej:
(define (apply0 g)
  (g 0))

; Cuidado con la aridad de las funciones, map entrega a la funcion 1 solo argumento
(define myList (list sub1 add1 (λ (x) (* 2 x)) (addn 212)
                     ;(λ (n m) (+ n m))  // ERROR: Arity mismatch - 2 arguments expected, 1 given
                     (addn 0)))

(map apply0 myList)

; + :: A x B -> C
; addn :: A -> B -> C === curry

; Ejercicio
; Definir funcion curry :: (A x B -> C) -> (A -> B -> C)
(define (currify f)
  (λ (a)
    (λ (b)
      (f a b))))

(define consify ((currify cons) 1))

(consify 2)
(consify 10)

; uncurry :: (A->B->C) -> (A x B) -> C
; genera una lambda de 2 argumentos y se los pasa en orden a f
(define (uncurry f)
  (λ (a b)
    ((f a) b)))

((uncurry (currify cons)) 1 2)

(test ((uncurry (currify cons)) 1 2) (cons 1 2))