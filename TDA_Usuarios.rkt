#lang racket
(require "Globales.rkt")

; ------------------------------- Usuarios -------------------------------

; ----------- Constructores -----------

; usuario = lista(credencial entero) ------ Definición: lista que contiene informacion para inicio de sesión(credencial) y reputacion(entero) del usuario

; credencial = lista(string_nombre string_contraseña) ------ Definición: lista que contiene el nombre de usuario y contraseña para inicio de sesión

; stack_usuarios = lista(usuarios) ------ Definición: lista que contiene la informacion para inicio de sesión y reputacion de cada usuario registrado


; ----------- Selectores -----------

(define (nombre credencial)(car credencial)) ; Descripcion: Funcion creada para obtener el nombre de usuario ---------- Dom: lista(string1 string2) ---------- Recorrido: string1
(define (contrasena credencial)(car (cdr credencial))) ; Descripcion: Funcion creada para obtener la contraseña de un usuario ---------- Dom: lista(string1 string2) ---------- Recorrido: string2
(define (credencial usuario)(car usuario)) ; Descripcion: Funcion creada para obtener la credencial del usuario ---------- Dom: lista(lista() entero) ---------- Recorrido: lista()
(define (reputacion usuario)(car (cdr usuario))) ; Descripcion: Funcion creada para obtener la reputación del usuario ---------- Dom: lista(string1 entero) ---------- Recorrido: entero
(define (usuario stack_usuarios)(car stack_usuarios)) ; Descripcion: Funcion creada para obtener el primer usuario de una lista de usuarios ---------- Dom: lista(usuarios) ---------- Recorrido: usuario


; --Funciones específicas--
; Para Credencial:
(define (getNombre_usuarios stack_usuarios)(nombre (credencial (usuario stack_usuarios)))) ; Descripcion: Funcion creada para obtener el nombre del primer usuario de una lista de usuarios ---------- Dom: lista(usuarios) ---------- Recorrido: string
(define (getContrasena_usuarios stack_usuarios)(contrasena (credencial (usuario stack_usuarios)))) ; Descripcion: Funcion creada para obtener la contraseña del primer usuario de una lista de usuarios ---------- Dom: lista(usuarios) ---------- Recorrido: string

; Para usuario:
(define (getCredencial_usuarios stack_usuarios)(credencial (usuario stack_usuarios))) ; Descripcion: Funcion creada para obtener la credencial del primer usuario en una lista de usuarios ---------- Dom: lista(usuarios) ---------- Recorrido: lista(credencial)
(define (getReputacion_usuarios stack_usuarios)(reputacion (usuario stack_usuarios))) ; Descripcion: Funcion creada para obtener la reputacion del primer usuario en una lista de usuarios ---------- Dom: lista(usuarios) ---------- Recorrido: entero

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
(define (usuarios? stack_usuarios)
  (if (vacioU? stack_usuarios)
      #t
      (if (usuario? (usuario stack_usuarios))
          (usuarios? (cdr stack_usuarios))
          #f)))

; Funciones importadas:
(provide getNombre_usuarios)
(provide getContrasena_usuarios)
(provide getCredencial_usuarios)
(provide getReputacion_usuarios)
(provide credencial?)
(provide usuario?)
(provide usuarios?)

;(all-defined-out)
;(provide (all-defined-out))

; --------------------------------------------------------------------------------------------------------------------------------------------
;(define lista (list (list (list "Nicolas" "123") 12) (list (list "Tania" "12") 23) (list (list "Pedro" "98") 76) (list (list "Jax" "87") 98)))
;(getNombre lista)
;(getContrasena lista)
;(getReputacion lista)
;(getCredencial lista)
;(credencial? (getCredencial lista))
;(usuario? (usuario lista))
;(usuarios? lista)
