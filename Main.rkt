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
                                                           (list (agregarPregunta_preguntas (getPreguntas_s lista_stack) (list (list 0 0) (getRespuestas_pregunta (+ 1 (len (getPreguntas_s lista_stack) 0)) listaR (list )) (+ 1 (len (getPreguntas_s lista_stack) 0)) lista_etiquetas titulo contenido lista_fecha (getUsuario_s lista_stack) 1 0 0 (list ))) (getRespuestas_s lista_stack) (agregarPregunta_usuarios (getUsuarios_s lista_stack) (getUsuario_s lista_stack) (list (list 0 0) (getRespuestas_pregunta (+ 1 (len (getPreguntas_s lista_stack) 0)) listaR (list )) (+ 1 (len (getPreguntas_s lista_stack) 0)) lista_etiquetas titulo contenido lista_fecha (getUsuario_s lista_stack) 1 0 0 (list )) (list )) ""))))) 

;(list (list 0 0) (getRespuestas_pregunta (+ 1 (len (getPreguntas_s lista_stack) 0)) listaR (list )) (+ 1 (len (getPreguntas_s lista_stack) 0)) lista_etiquetas titulo contenido lista_fecha (getUsuario_s lista_stack) 1 0 0)


; -------------------- reward --------------------


(define (agregarRec_preguntas lista_preguntas ID Rec usuario lista)
  (if (vacio? lista_preguntas)
      lista
      (if (= (getID_p (car lista_preguntas)) ID)
          (agregarRec_preguntas (cdr lista_preguntas) ID Rec usuario (cons (list (getVotos_p (car lista_preguntas)) (getRespuestas_p (car lista_preguntas)) (getID_p (car lista_preguntas)) (getEtiquetas_p (car lista_preguntas)) (getTitulo_p (car lista_preguntas)) (getContenido_p (car lista_preguntas)) (getFecha_p (car lista_preguntas)) (getAutor_p (car lista_preguntas)) (getEstado_p (car lista_preguntas)) (getRecompensa_p (car lista_preguntas)) (getReporte_p (car lista_preguntas)) (cons (list usuario Rec) (getRecompensasR_p (car lista_preguntas)))) lista))
          (agregarRec_preguntas (cdr lista_preguntas) ID Rec usuario (cons (car lista_preguntas) lista)))))
  
  
(define (agregarRec_usuarios lista_usuarios lista_preguntas Rec usuario lista ID_pregunta)
  (if (vacio? lista_usuarios)
      lista
      (if (equal? (car (getCredencial_u (car lista_usuarios))) usuario)
          (cons (list (getCredencial_u (car lista_usuarios)) (getPreguntas_usuario usuario (agregarRec_preguntas lista_preguntas ID_pregunta Rec usuario (list )) (list )) (getRespuestas_u (car lista_usuarios)) (- (getReputacion_u (car lista_usuarios)) Rec)) lista)
          (cons (car lista_usuarios) lista))))


(define (reward lista_stack)(lambda (ID_pregunta)(lambda (recompensa)
                                                   (if (equal? (getUsuario_s lista_stack) "")
                                                       lista_stack
                                                       (if (<= recompensa (getReputacion_u (getUsuario_usuario (getUsuario_s lista_stack) (getUsuarios_s lista_stack))))
                                                           (list (agregarRec_preguntas (getPreguntas_s lista_stack) ID_pregunta recompensa (getUsuario_s lista_stack) (list )) (getRespuestas_s lista_stack) (agregarRec_usuarios (getUsuarios_s lista_stack) (getPreguntas_s lista_stack) recompensa (getUsuario_s lista_stack) (list ) ID_pregunta) "")
                                                           (list (getPreguntas_s lista_stack) (getRespuestas_s lista_stack) (getUsuarios_s lista_stack) ""))))))


;(((login stack "Usuario1" "contrasena1" reward) 2) 300)
;(getUsuario_usuario "Usuario1" (getUsuarios_s (((login stack "Usuario1" "contrasena1" reward) 2) 300)))


; -------------------- answer --------------------

(define (estaID? ID preguntas)
  (if (vacio? preguntas)
      #f
      (if (= ID (getID_p (car preguntas)))
          #t
          (estaID? ID (cdr preguntas)))))

(define (agregarRespuesta_R_a lista_respuestas respuesta)(cons respuesta lista_respuestas))

(define (agregarRespuesta_P_a lista_preguntas lista_respuestas ID respuesta lista)
  (if (vacio? lista_preguntas)
      lista
      (if (= (getID_p (car lista_preguntas)) ID)
          (agregarRespuesta_P_a (cdr lista_preguntas) lista_respuestas ID respuesta (cons (list (getVotos_p (car lista_preguntas)) (cons respuesta (getRespuestas_p (car lista_preguntas))) (getID_p (car lista_preguntas)) (getEtiquetas_p (car lista_preguntas)) (getTitulo_p (car lista_preguntas)) (getContenido_p (car lista_preguntas)) (getFecha_p (car lista_preguntas)) (getAutor_p (car lista_preguntas)) (getEstado_p (car lista_preguntas)) (getRecompensa_p (car lista_preguntas)) (getReporte_p (car lista_preguntas)) (getRecompensasR_p (car lista_preguntas))) lista))
          (agregarRespuesta_P_a (cdr lista_preguntas) lista_respuestas ID respuesta (cons (car lista_preguntas) lista)))))
       
(define (agregarRespuesta_U lista_usuarios lista_preguntas lista_respuestas ID respuesta usuario lista)
  (if (vacio? lista_usuarios)
      lista
      (if (equal? (car (getCredencial_u (car lista_usuarios))) usuario)
          (agregarRespuesta_U (cdr lista_usuarios) lista_preguntas lista_respuestas ID respuesta usuario (cons  (list (getCredencial_u (car lista_usuarios)) (getPreguntas_usuario usuario (agregarRespuesta_P_a lista_preguntas lista_respuestas ID respuesta (list )) (list )) (cons respuesta (getRespuestas_u (car lista_usuarios))) (getReputacion_u (car lista_usuarios))) lista))
          (agregarRespuesta_U (cdr lista_usuarios) lista_preguntas lista_respuestas ID respuesta usuario (cons  (list (getCredencial_u (car lista_usuarios)) (getPreguntas_usuario usuario (agregarRespuesta_P_a lista_preguntas lista_respuestas ID respuesta (list )) (list )) (getRespuestas_u (car lista_usuarios)) (getReputacion_u (car lista_usuarios))) lista)))))

  

(define (answer lista_stack)(lambda (fecha)(lambda (ID_pregunta)(lambda (respuesta)
                                                                  (if (equal? (getUsuario_s lista_stack) "")
                                                                      lista_stack
                                                                      (if (estaID? ID_pregunta (getPreguntas_s lista_stack))
                                                                          (list (agregarRespuesta_P_a (getPreguntas_s lista_stack) (getRespuestas_s lista_stack) ID_pregunta (list (getUsuario_s lista_stack) fecha ID_pregunta (+ (len (getRespuestas_pregunta ID_pregunta (getRespuestas_s lista_stack) (list )) 0) 1)  (list 0 0) 1 0 respuesta 0) (list )) (agregarRespuesta_R_a (getRespuestas_s lista_stack) (list (getUsuario_s lista_stack) fecha ID_pregunta (+ 1 (len (getRespuestas_pregunta ID_pregunta (getRespuestas_s lista_stack) (list )) 0))  (list 0 0) 1 0 respuesta 0)) (agregarRespuesta_U (getUsuarios_s lista_stack) (getPreguntas_s lista_stack) (getRespuestas_s lista_stack) ID_pregunta (list (getUsuario_s lista_stack) fecha ID_pregunta (+ 1 (len (getRespuestas_pregunta ID_pregunta (getRespuestas_s lista_stack) (list )) 0))  (list 0 0) 1 0 respuesta 0) (getUsuario_s lista_stack) (list )))
                                                                          lista_stack))))))

;((((login stack "Usuario1" "contrasena1" answer) (list 25 10 2020)) 6) "Mi respuesta")
;(getPregunta_ID 132 (getPreguntas_s ((((login stack "Usuario1" "contrasena1" answer) (list 25 10 2020)) 132) "Mi respuesta")))


; -------------------- accept --------------------


;(list (getVotos_p (car lista_preguntas)) (cons respuesta (getRespuestas_p (car lista_preguntas))) (getID_p (car lista_preguntas)) (getEtiquetas_p (car lista_preguntas)) (getTitulo_p (car lista_preguntas)) (getContenido_p (car lista_preguntas)) (getFecha_p (car lista_preguntas)) (getAutor_p (car lista_preguntas)) (getEstado_p (car lista_preguntas)) (getRecompensa_p (car lista_preguntas)) (getReporte_p (car lista_preguntas)) (getRecompensasR_p (car lista_preguntas)))
;(list (getUsuario_s lista_stack) fecha ID_pregunta (+ 1 (len (getRespuestas_pregunta ID_pregunta (getRespuestas_s lista_stack) (list )) (list )))  (list 0 0) 1 0 respuesta 0)                                                                      
                                                                  


;(list (getVotos_p (car lista_preguntas)) (getRespuestas_p (car lista_preguntas)) (getID_p (car lista_preguntas)) (getEtiquetas_p (car lista_preguntas)) (getTitulo_p (car lista_preguntas)) (getContenido_p (car lista_preguntas)) (getFecha_p (car lista_preguntas)) (getAutor_p (car lista_preguntas)) (getEstado_p (car lista_preguntas)) (getRecompensa_p (car lista_preguntas)) (getReporte_p (car lista_preguntas)) (cons (list usuario Rec) (getRecompensasR_p (car lista_preguntas))))
;(car lista_preguntas)
;(getVotos_p (car lista_preguntas)) (getRespuestas_p (car lista_preguntas)) (getID_p (car lista_preguntas)) (getEtiquetas_p (car lista_preguntas)) (getTitulo_p (car lista_preguntas)) (getContenido_p (car lista_preguntas)) (getFecha_p (car lista_preguntas)) (getAutor_p (car lista_preguntas)) (getEstado_p (car lista_preguntas)) (getRecompensa_p (car lista_preguntas)) (getReporte_p (car lista_preguntas)) (cons (list usuario Rec) (getRecompensasR_p (car lista_preguntas)))


;(((login stack "Usuario1" "contrasena1" reward) 2) 300)

;(getPreguntas_s (((login stack "Usuario1" "contrasena1" reward) 2) 300))
;(getPregunta_ID 2 (getPreguntas_s (((login stack "Usuario1" "contrasena1" reward) 2) 300)))

; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;(getUsuarios_s stack)
;(getUsuarios_s (register stack "Nacho" "contranacho"))

;(credencialValida? (getUsuarios_s stack) "Usuario1" "contrasena1")

;(getPreguntas_s (((login stack "Usuario1" "contrasena1" ask) (list 2 3 2020)) "titulo" (list "etiqueta1" "etiqueta2" "etiqueta3") "contenido"))

;(getPreguntas_s (((login stack "Usuario1" "contrasena1" ask) (list 2 3 2020)) "titulo" (list "etiqueta1" "etiqueta2" "etiqueta3") "contenido"))
;(getUsuario_s (((login stack "Usuario1" "contrasena1" ask) (list 2 3 2020)) "titulo" (list "etiqueta1" "etiqueta2" "etiqueta3") "contenido"))

;(list (list 0 0);(getRespuestas_pregunta (+ 1 (len (getPreguntas_s lista_stack) 0)) listaR (list )) (+ 1 (len (getPreguntas_s lista_stack) 0)) lista_etiquetas titulo contenido lista_fecha (getUsuario_s lista_stack) 1 0 0 (list ))