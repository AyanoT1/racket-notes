#lang play

#|
 Estructuras recursivas

 Naturales
 <nat> ::= 0
        | S <nat> // sucesor natural

 Se pueden hacer razonamientos inductivos sobre las estructuras recursivas
 ∀n P(n) • P(0)
         • P(n-1)=>P(n)

 Listas
 <list> ::= '()
         | (cons <val> <list>)
|#

; Suma de Gauss
; sum : nat -> nat
(define (sum n)
  (if (zero? n)
      0                       ; caso base
      (+ n (sum (sub1 n)))))  ; caso inductivo

; (sum 0) ; 0
; (sum 1) ; 1
; (sum 3) ; 6
; (sum 100) ; 5050
; (sum 87912) ; 3864303828


(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))

; (factorial 102)


#|
 Estructura de funciones sobre listas
 (define (f l)
   (if (empty? l))
     ...
     ... (f (rest l) ...))
|#

(define (len l)
  (if (empty? l)
      0
      (add1 (len (rest l)))))

; (len (list 1 2 3 4 5)) ; 5

; l: list nat
(define (lmax l)
  (if (empty? l)
      0
      (max (first l) (lmax (rest l)))))

; (lmax (list 5 4 100 2 1)) ; 100


#|
 ⚠️ IMPLEMENTACIÓN PENCA: se cae cuando no tiene 2 hojas cada arbol ⚠️


 <btree> ::= <val> ; <val> no puede ser lista
          | (list <val> <btree> <btree>)

  ejemplo:
|#
; (list 1 (list 2 5 6) 3)

; tmax : List[Num] -> Num
(define (tmax t)
  (if (not (list? t))  ; "discriminador hoja vs nodo"
      t
      (max (first t) (tmax (second t)) (tmax (third t)))))


; (tmax (list 1 (list 2 5 6) 3)) ; 6
; (tmax (list 1 (list 2 5 16) (list 3 4 6))) ; 16
; (tmax (list 1 2)) ; ❌ Caso problemático


;; PLAY (#lang play) exclusivo
#|
 <btree> ::= (leaf <val>)
          | (node <val> <btree> <btree>)

 deftype es una abstracción de structs de play para definir tipos algegráicos fácilmente

 ℹ️ leaf y node no son tipos, solo constructores, el único tipo es BTree ℹ️
|#
(deftype BTree
  (leaf val)             ; constructor 1: leaf, toma 1 arg
  (node val left right)) ; constructor 2: node, toma 3 args

(define l (leaf 10))
(define bt (node 1
                 (node 2
                       (leaf 3)
                       (leaf 4))
                 (node 5
                       (node 7
                             (leaf 8)
                             (leaf 9))
                       (leaf 10))))

#|
 se generan:
  - accesores: <cstr>-<field> leaf-val node-val node-left node-right
  ⚠️ los accesores como leaf-val son funciones, no metodos de objetos
    se ocupan como (leaf-val l) ⚠️

  - predicados: BTree?
 |#

; (node-val (node-left t)) ; 2

; (BTree? 10)        ; #f
; (BTree? (leaf 10)) ; #t
; (leaf? (leaf 10))  ; #t


;  😼 Hay pattern matching 😼

; (match 10
;   [(? even?) 'ok]
;   [else      'no]) ; 'ok

; (match 11
;   [(? even?) 'ok]
;   [else      'no]) ;'no

; (match 11
;   [(? even?) 'ok]
;   [11        'bingo]
;   [else      'no]) ; 'bingo

; (match 17
;   [(? even?) 'ok]
;   [11        'bingo]
;   [else      (* 2 else)]) ; 34 ⚠️ else es el nombre de la variable entrante, no es una keyword ⚠️

; (match bt
;   [(leaf v) v]
;   [e e]) ; (node 1 (node 2 (leaf 3) (leaf 4)) (node 5 (node 7 (leaf 8) (leaf 9)) (leaf 10)))

; (match (node-right (node-right bt))
;   [(leaf v) v]
;   [e e]) ; 10

; (match (node-right bt)
;   [(node v1 (node v2 l2 r2) _) (+ v1 v2)]
;   [e e]) ; 12

#|
  patrón de funciones sobre BTree
  ; f :: BTree -> some
  (define (f t)
    (match t
      [(leaf v) ...]
      [(node v l r) ... (f l) ... (f r)]))
|#

; btmax :: BTree -> Num
(define (btmax t)
  (match t
    [(leaf v) v]
    [(node v l r) (max v (btmax l) (btmax r))]))

; (btmax bt) ; 10


; TODO: agregar un unary-node, nodos que solo tienen un hijo
; TODO: agregar un nodo n-ario (una lista de hijos) // ℹ️ usar map