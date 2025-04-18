#lang play

; Más problemas de sustitución, en especifico la ineficiencia de la sustitución por cada variable.
; se recorre el AST cada vez que se sustituye una varialbe, luego cuando introducimos memoria
; están los problemas de scope como en el programa

(let [(x 1)]
  (+ (let [(x 2)] x) x))

; El resultado es 3, pero no es trivial guardar las variables por ejemplo una lookup table,
; x puede valer 1 o 2 dependiendo del scope y se pueden ocupar en distintos momentos.
; En cada scope sólo hay que sustituir las ocurrencias libres (unbound).

; Recordar que en Racket, las variables son inmutables!

; Nota: una OPTIMIZACIÓN debe mantener la semántica, al igual que un REFACTOR.