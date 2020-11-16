#lang racket
(require "Globales.rkt")
(require "TDA_Respuestas.rkt")
(require "TDA_Preguntas.rkt")
(require "TDA_Usuarios.rkt")


; ------------------------------- Stack -------------------------------


; ----------- Constructores -----------


; stack = lista(lista(preguntas) lista(respuestas) lista(usuarios) usuario)

; lista(preguntas) = lista de preguntas ------ Definición: lista que contiene las preguntas echas por todos los usuarios registrados
; lista(respuestas) = lista de respuestas ------ Definición: lista que contiene todas las respuestas echas por los usuarios registrados
; lista(usuarios) = lista de usuarios ------ Definición: lista que contiene a todos los usuarios registardos
; usuario = string ------ Definición: string que señala el usuario activo, se utiliza "" para la funcion "register"


; ----------- Selectores -----------


(define (getPreguntas_s lista_stack)(car lista_stack))
(define (getRespuestas_s lista_stack)(car (cdr lista_stack)))
(define (getUsuarios_s lista_stack)(car (cdr (cdr lista_stack))))
(define (getUsuario_s lista_stack)(car (cdr (cdr (cdr lista_stack)))))


; ----------- Pertenencias -----------


(define (stack? lista_stack)
  (if (and (preguntas? (getPreguntas_s lista_stack)) (respuestas? (getRespuestas_s lista_stack)) (usuarios? (getUsuarios_s lista_stack)) (string? (getUsuario_s lista_stack)))
      #t
      #f))


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


(define stack(list listaP listaR listaU ""))


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


(provide getPreguntas_s)
(provide getRespuestas_s)
(provide getUsuarios_s)
(provide getUsuario_s)
(provide stack)


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

