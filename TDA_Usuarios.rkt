#lang racket
; ------------------------------- Usuarios -------------------------------

; ----------- Constructores -----------

; usuario = lista(credencial entero) ------ Definición: lista que contiene informacion para inicio de sesión(credencial) y reputacion(entero) del usuario

; credencial = lista(string_nombre string_contraseña) ------ Definición: lista que contiene el nombre de usuario y contraseña para inicio de sesión

; stack_usuarios = lista(usuarios) ------ Definición: lista que contiene la informacion para inicio de sesión y reputacion de cada usuario registrado


; ----------- Selectores -----------

; Para Credencial:
(define (nombre credencial)(car credencial)) ; Descripcion: Funcion creada para obtener el nombre de usuario ---------- Dom: lista(string1 string2) ---------- Recorrido: string1

(define (contrasena credencial)(car (cdr credencial))) ; Descripcion: Funcion creada para obtener la contraseña de un usuario ---------- Dom: lista(string1 string2) ---------- Recorrido: string2

; Para usuario:
(define (credencial usuario)(car usuario)) ; Descripcion: Funcion creada para obtener la credencial del usuario ---------- Dom: lista(lista() entero) ---------- Recorrido: lista()

(define (reputacion usuario)(car (cdr usuario))) ; Descripcion: Funcion creada para obtener la reputación del usuario ---------- Dom: lista(string1 entero) ---------- Recorrido: entero

; Para usuarios:
(define (usuario usuarios)(car usuarios))

; Funciones específicas:
(define (getNombre usuarios)(nombre (credencial (usuario usuarios))))
(define (getContrasena usuarios)(contrasena (credencial (usuario usuarios))))
(define (getCredencial usuarios)(credencial (usuario usuarios)))
(define (getReputacion usuarios)(reputacion (usuario usuarios)))

; ----------- Pertenencias -----------
; Para listas vacías
(define (vacioU? lista)
  (if (null? lista)
      #t
      #f))

; Para Credencial:
(define (credencial? credenciall)
  (if (and (string? (nombre credenciall)) (string? (contrasena credenciall)))
      #t
      #f))

; Para usuario:
(define (usuario? usuario)
  (if (and (credencial? (credencial usuario)) (integer? (reputacion usuario)))
      #t
      #f))

; Para usuarios:
(define (usuarios? usuarioss)
  (if (vacioU? usuarioss)
      #t
      (if (usuario? (usuario usuarioss))
          (usuarios? (cdr usuarioss))
          #f)))

; --------------------------------------------------------------------------------------------------------------------------------------------
(define lista (list (list (list "Nicolas" "123") 12) (list (list "Tania" "12") 23) (list (list "Pedro" "98") 76) (list (list "Jax" "87") 98)))
;(getNombre lista)
;(getContrasena lista)
;(getReputacion lista)
;(getCredencial lista)
;(credencial? (getCredencial lista))
;(usuario? (usuario lista))
;(usuarios? lista)
