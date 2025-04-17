#lang play


; P2) Lifting
(deftype Value
  (numV n)
  (boolV b)
  (pairV l r)
  )

;; interp-expr :: Expr* -> Value


; liftNum+ :: Value * Value -> Value
(define (liftNum+ v1 v2)
  (match-let ([(numV n1) v1]
              [(numV n2) v2])
    (numV (+ n1 n2))))

; (liftNum+ (numV 10) (numV 4))

; liftBool&& :: Value * Value -> Value
(define (liftBool&& v1 v2)
  (match-let ([(boolV b1) v1] [(boolV b2) v2]) (boolV (and b1 b2))))

; (liftBool&& (boolV #t) (boolV #t))
; (liftBool&& (boolV #t) (boolV #f))
; (liftBool&& (boolV #f) (boolV #t))
; (liftBool&& (boolV #f) (boolV #f))

