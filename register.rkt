#lang racket
; -------------------- Selectores --------------------
(define (getName usuario)(car usuario)) ; Descripcion: Funcion creada para obtener el nombre de usuario ---------- Dom: lista(string1 string2) ---------- Recorrido: string1

(define (getContrasena usuario)(car (cdr usuario))) ; Descripcion: Funcion creada para obtener la contraseña de un usuario ---------- Dom: lista(string1 string2) ---------- Recorrido: string2

(define (vacio? lista)(if (null? lista) ; Descripcion: Funcion creada para ver si una lista está vacía ---------- Dom: lista ---------- Recorrido: True si está vacía, False si tiene elementos incorporados
                          #t
                          #f))

(define (noEsta? stack usuario)
  (if (vacio? stack)
      #t
      (if (equal? (getName (car stack)) usuario)
          #f
          (noEsta? (cdr stack) usuario))))

(define (register stack usuario contrasena)
  (if (noEsta? stack usuario);
      (cons (list usuario contrasena) lista)
      "Usuario ya registrado"))
      
;(noEsta? (list (list "Nicolas" 123) (list "Tania" 12) (list "Pedro" 98) (list "Jax" 87)) "Nicolas")
(define lista (list (list "Nicolas" "123") (list "Tania" "12") (list "Pedro" "98") (list "Jax" "87")))
(register lista "Nicolas" "123")
;(getContrasena (list "Nicola" "123"))