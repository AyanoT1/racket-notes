#lang play

; Ejercicios ez

#|
  fib :: int -> int
  computes the n'th number of the fibonacci sequence
|#
; (define (fib n)
;   (cond [(<= 0)]))
; ; ...

; (test (fib 0) 0)
; (test (fib 1) 1)
; (test (fib 2) 3)

;; concat-strings :: str[] -> str
;; Joins the arguments together into a string
(define (concat-strings arr)
  (foldr string-append "" arr)
  ; (foldl (λ (s acc) (string-append acc s)) "" arr)  ; This is more efficient, for some reason
  )

(test (concat-strings (list "1" "2" "3" "4")) "1234")
(test (concat-strings (list "1" "2" "3")) "123")
(test (concat-strings (list "skibidi" " " "toilet")) "skibidi toilet")

; (define (compose f g)
;   (λ (x) (f (g x))))


; list-range :: int int -> int[]
; Returns int interval between both, first inclusive, last exclusive
(define (list-range i j)
  (if (>= i j)
      '()
      (cons i (list-range (add1 i) j))))

; skibidi-buzz int -> int | str
(define (skibidi-buzz n)
  (cond
    [(= 0 (modulo n 15)) "fizzbuzz"]
    [(= 0 (modulo n 3)) "fizz"]
    [(= 0 (modulo n 5)) "buzz"]
    [else n]))

; fizzbuzz :: int -> (str | int)[]
(define (fizzbuzz i)
  (map skibidi-buzz (list-range 1 (add1 i))))

(test (fizzbuzz 15) (list 1 2 "fizz" 4 "buzz" "fizz" 7 8 "fizz" "buzz" 11 "fizz" 13 14 "fizzbuzz"))


(deftype Nat
  (zero)
  (S n))
(define O (zero)) 

(define (peano-sum n1 n2)
  (match n1
    [(zero) n2]
    [(S n) (S (peano-sum n n2))]
    ))

; (peano-sum (S O) O)

(define (peano-mult n1 n2)
  (match n1
    [(zero) O]
    [(S n) (peano-sum n2) (peano-mult n n2)]
    ))


(define (div2 n)
  (match n
    [(zero) O]
    [(S (zero)) (zero)]
    [(S (S n)) (S (div2 n))]
    ))

(test (div2 O) O)
(test (div2 (S O)) O)
(test (div2 (S (S O))) (S O))

(deftype Tree23
  (leaf n)
  (node2 l n r)
  (node3 l n1 m n2 r))

; tree-height :: Tree23 -> int
; Returns the height of the Tree23
(define (tree-height t)
    (match t
      [(leaf n) 1]
      [(node2 l n r) (add1 (max (tree-height l) (tree-height r)))]
      [(node3 l n1 m n2 r) (add1 (max (tree-height l) (tree-height m) (tree-height r)))]
    ))

(define (is-ordered x y t)
(match t
  [(leaf n) (and (>= n x) (> y n))]
  [(node2 l n r) (and (<= x n) (< n y) (is-ordered x n l) (is-ordered n y r))]
  [(node3 l n1 m n2 r) (and (<= x n1) (< n1 n2 y) (is-ordered x n1 l) (is-ordered n1 n2 m) (is-ordered n2 y r))]
  ))