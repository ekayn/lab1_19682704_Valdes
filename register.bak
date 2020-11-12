#lang racket
; -------------------- Selectores --------------------

(define (getName usuario)(car usuario))  
(define (vacio? algo)(= algo null))  


(define (noEsta? stack usuario)
  (if (null? stack)
      #t
      (if (equal? (getName (car stack)) usuario)
          #f
          (noEsta? (cdr stack) usuario))))

(define (register stack usuario contrasena)
  (if (noEsta? usuario stack)
      

;(noEsta? (list (list "Nicolas" 123) (list "Tania" 12) (list "Pedro" 98) (list "Jax" 87)) "Nicolas")
