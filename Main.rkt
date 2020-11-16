#lang racket
(require "Globales.rkt")
(require "TDA_Respuestas.rkt")
(require "TDA_Preguntas.rkt")
(require "TDA_Usuarios.rkt")
(require "TDA_Stack.rkt")


; -------------------- Register --------------------


(define (register lista_stack string_usuario string_contrasena)
  (if (usuarioValido? string_usuario (getUsuarios_s lista_stack))
      (list (getPreguntas_s lista_stack) (getRespuestas_s lista_stack) (cons (list (list string_usuario string_contrasena) (getPreguntas_usuario string_usuario (getPreguntas_s lista_stack) (list )) (getRespuestas_usuario string_usuario (getRespuestas_s lista_stack) (list )) 0) (getUsuarios_s lista_stack)) "")
      lista_stack))


; -------------------- Login --------------------


(define (credencialValida? lista_usuarios usuario contrasena)
  (if (vacio? lista_usuarios)
      #f
      (if (and (equal? (car (getCredencial_u (car lista_usuarios))) usuario) (equal? (car (cdr (getCredencial_u (car lista_usuarios)))) contrasena))
          #t
          (credencialValida? (cdr lista_usuarios) usuario contrasena))))

(define (login lista_stack usuario contrasena operacion)
  (if (credencial_u? (list usuario contrasena))
      (if (credencialValida? (getUsuarios_s lista_stack) usuario contrasena)
          (operacion (list (getPreguntas_s lista_stack) (getRespuestas_s lista_stack) (getUsuarios_s lista_stack) usuario))
          (operacion lista_stack))
      (operacion lista_stack)))


; -------------------- ask --------------------


(define (agregarPregunta_preguntas lista_preguntas pregunta)(cons pregunta lista_preguntas))

(define (agregarPregunta_usuarios lista_usuarios usuario pregunta lista)
  (if (vacio? lista_usuarios)
      lista
      (if (equal? (car (getCredencial_u (car lista_usuarios))) usuario)
          (agregarPregunta_usuarios (cdr lista_usuarios) usuario pregunta (cons (list (getCredencial_u (car lista_usuarios)) (cons pregunta (getPreguntas_u (car lista_usuarios))) (getRespuestas_u (car lista_usuarios)) (getReputacion_u (car lista_usuarios))) lista))
          (agregarPregunta_usuarios (cdr lista_usuarios) usuario pregunta (cons (car lista_usuarios) lista)))))
  

(define (ask lista_stack)(lambda (lista_fecha)(lambda (titulo lista_etiquetas contenido)
                                                       (if (equal? (getUsuario_s lista_stack) "")
                                                           lista_stack
                                                           (list (agregarPregunta_preguntas (getPreguntas_s lista_stack) (list (list 0 0) (getRespuestas_pregunta (+ 1 (len (getPreguntas_s lista_stack) 0)) listaR (list )) (+ 1 (len (getPreguntas_s lista_stack) 0)) lista_etiquetas titulo contenido lista_fecha (getUsuario_s lista_stack) 1 0 0)) (getRespuestas_s lista_stack) (agregarPregunta_usuarios (getUsuarios_s lista_stack) (getUsuario_s lista_stack) (list (list 0 0) (getRespuestas_pregunta (+ 1 (len (getPreguntas_s lista_stack) 0)) listaR (list )) (+ 1 (len (getPreguntas_s lista_stack) 0)) lista_etiquetas titulo contenido lista_fecha (getUsuario_s lista_stack) 1 0 0) (list )) ""))))) 

;(list (list 0 0) (getRespuestas_pregunta (+ 1 (len (getPreguntas_s lista_stack) 0)) listaR (list )) (+ 1 (len (getPreguntas_s lista_stack) 0)) lista_etiquetas titulo contenido lista_fecha (getUsuario_s lista_stack) 1 0 0)


; -------------------- reward --------------------





; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(define listaT(list listaU listaP listaR))

; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;(getUsuarios_s stack)
;(getUsuarios_s (register stack "Nacho" "contranacho"))

;(credencialValida? (getUsuarios_s stack) "Usuario1" "contrasena1")

;(((login stack "Usuario1" "contrasena1" ask) (list 2 3 2020)) "titulo" (list "etiqueta1" "etiqueta2" "etiqueta3") "contenido")
;(getPreguntas_s (((login stack "Usuario1" "contrasena1" ask) (list 2 3 2020)) "titulo" (list "etiqueta1" "etiqueta2" "etiqueta3") "contenido"))
;(getUsuario_s (((login stack "Usuario1" "contrasena1" ask) (list 2 3 2020)) "titulo" (list "etiqueta1" "etiqueta2" "etiqueta3") "contenido"))
