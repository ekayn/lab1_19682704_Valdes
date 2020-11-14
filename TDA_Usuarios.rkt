#lang racket
(require "Globales.rkt")
(require "TDA_Respuestas.rkt")
(require "TDA_Preguntas.rkt") 


; ------------------------------- Usuarios -------------------------------


; ----------- Constructores -----------

; usuarios = lista(usuarios) ------ Definición: lista que contiene a todos los usuarios registardos

; usuario = lista(lista(credencial) lista(preguntas) lista(respuestas) reputación)  ------ Definición: lista que contiene informacion para inicio de sesión(credencial), preguntas echas por el usuario(lista(preguntas)), respuestas echas por el usuario(lista(respuestas)) y reputacion del usuario
; credencial = lista(string_nombre string_contraseña) ------ Definición: lista que contiene el nombre de usuario y contraseña para inicio de sesión
; lista(preguntas) = listado de preguntas ------ Definición: listado de preguntas echas por el usuario
; lista(respuestas) = lista de respuestas ------ Definición: listado de respuestas echas por el usuario
; reputación = entero ------ Definición: entero que señala la reputacion del usuario


; ----------- Selectores -----------


(define (getCredencial_u usuario)(car usuario)) ; Descripcion: Funcion creada para obtener la credencial del usuario ---------- Dom: lista_usuario(list()) ---------- Recorrido: lista(nombre_usuario contraseña)
(define (getPreguntas_u usuario)(car (cdr usuario))) ; Descripcion: Funcion creada para obtener todas las preguntas echas por el usuario ---------- Dom: lista_usuario(list()) ---------- Recorrido: lista_preguntas(list())
(define (getRespuestas_u usuario)(car (cdr (cdr usuario)))) ; Descripcion: Funcion creada para obtener las respuestas echas por el usuario ---------- Dom: lista_usuario(list()) ---------- Recorrido: lista_respuestas(list())
(define (getReputacion_u usuario)(car (cdr (cdr (cdr usuario))))) ; Descripcion: Funcion creada para obtener la reputacion del usuario ---------- Dom: lista_usuario(list()) ---------- Recorrido: entero

                                           
; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


; Selectores específicos:
(define (getUsuario_usuario usuario lista_usuarios)
  (if (equal? (car (getCredencial_u (car lista_usuarios))) usuario)
      (car lista_usuarios)
      (getUsuario_usuario usuario (cdr lista_usuarios))))

                                           
; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                          
                                           
; ----------- Pertenencias -----------


(define (credencial_u? credencial)
  (if (and (string? (car credencial)) (string? (car (cdr credencial))))
      #t
      #f))

(define (reputacion_u? entero_reputacion)
  (if (integer? entero_reputacion)
      #t
      #f))

(define (usuario? lista_usuario)
  (if (and (not(vacio? lista_usuario)) (= (len lista_usuario 0) 4))
      (if (and (credencial_u? (car lista_usuario)) (preguntas? (car (cdr lista_usuario))) (respuestas? (car (cdr (cdr lista_usuario)))) (reputacion_u? (car (cdr (cdr (cdr lista_usuario))))))
          #t
          #f)
      #f))

(define (usuarios? lista_usuarios)
  (if (vacio? lista_usuarios)
      #t
      (if (usuario? (car lista_usuarios))
          (usuarios? (cdr lista_usuarios))
          #f)))

; --------------------------------------------------------------------------------------------------------------------------------------------


(define usuario1(list (list "Usuario1" "contrasena1") (getPreguntas_usuario "Usuario1" listaP (list )) (getRespuestas_usuario "Usuario1" listaR (list )) 10))
(define usuario2(list (list "Usuario2" "contrasena2") (getPreguntas_usuario "Usuario2" listaP (list )) (getRespuestas_usuario "Usuario2" listaR (list )) 20))
(define usuario3(list (list "Usuario3" "contrasena3") (getPreguntas_usuario "Usuario3" listaP (list )) (getRespuestas_usuario "Usuario3" listaR (list )) 30))
(define usuario4(list (list "Usuario4" "contrasena4") (getPreguntas_usuario "Usuario4" listaP (list )) (getRespuestas_usuario "Usuario4" listaR (list )) 40))
(define usuario5(list (list "Usuario5" "contrasena5") (getPreguntas_usuario "Usuario5" listaP (list )) (getRespuestas_usuario "Usuario5" listaR (list )) 50))

(define listaU(list usuario1 usuario2 usuario3 usuario4 usuario5))


; --------------------------------------------------------------------------------------------------------------------------------------------


(provide usuario?)
(provide usuarios?)

(provide listaU)


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
