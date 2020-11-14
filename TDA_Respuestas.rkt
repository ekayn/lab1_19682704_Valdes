#lang racket
(require "Globales.rkt")

; ------------------------------- Respuestas -------------------------------

; ----------- Constructores -----------

; respuestas = lista(respuesta) ------ Definición: lista que contiene todas las respuestas echas por los usuarios registrados

; respuesta = lista(string(usuario) lista(fecha) entero_ID entero_correlativo lista(votos) entero_estado entero_reportes string(respuesta))
      ; string(usuario) = string ------ Definición: string que señala el nombre del usuario que respondio
      ; lista(fecha) = lista(dia mes ano) ------ Definición: lista de enteros que contiene la fecha en la que se registro la respuesta
      ; entero_ID = entero ------ Definición: entero que registra la direccion ID de la pregunta que se respondio
      ; entero_correlativo = entero ------ Definición: entero que señala la posicion de registro de la respuesta en la pregunta
      ; lista(votos) = lista(voto1 voto2) ------ Definición: lista que contiene la cantidad de votos positivos(voto1) y la cantidad de votos negativos(voto2)
      ; entero_estado = entero ------ Definición: entero que señala si la respuesta es aseptada(1) o rechazada(0)
      ; entero_reportes = entero ------ Definición: entero que señala la cantidad de reportes de spam u ofensa
      ; string(respuesta) = string ------ Definición: string que señala la respuesta publicada por el usuario


; ----------- Selectores -----------

; --Funciones específicas--
(define (getNombre_r respuesta)(car respuesta))
(define (getFecha_r respuesta)(car (cdr respuesta)))
(define (getID_r respuesta)(car (cdr (cdr respuesta))))
(define (getCorrelativo_r respuesta)(car (cdr (cdr (cdr respuesta)))))
(define (getVotos_r respuesta)(car (cdr (cdr (cdr (cdr respuesta))))))
(define (getEstado_r respuesta)(car (cdr (cdr (cdr (cdr (cdr respuesta)))))))
(define (getReportes_r respuesta)(car (cdr (cdr (cdr (cdr (cdr (cdr respuesta))))))))
(define (getRespuesta_r respuesta)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr respuesta)))))))))

; Selectores específicos:
(define (getRespuestas_usuario usuario respuestas lista)
  (if (vacio? respuestas)
      (reversed lista (list ))
      (if (equal? usuario (getNombre_r (car respuestas)))
                  (getRespuestas_usuario usuario (cdr respuestas) (cons (car respuestas) lista))
                  (getRespuestas_usuario usuario (cdr respuestas) lista))))

(define (getRespuestas_pregunta ID respuestas lista)
  (if (vacio? respuestas)
      (reversed lista (list ))
      (if (= ID (getID_r (car respuestas)))
                  (getRespuestas_pregunta ID (cdr respuestas) (cons (car respuestas) lista))
                  (getRespuestas_pregunta ID (cdr respuestas) lista))))

; ----------- Pertenencias -----------

(define (nombre_r? string_usuario)
  (if (string? string_usuario)
      #t
      #f))

(define (fecha_r? lista_fecha)
  (if (and (not(vacio? lista_fecha)) (= (len lista_fecha 0) 3))
      (if (and (integer? (car lista_fecha)) (integer? (car (cdr lista_fecha))) (integer? (car (cdr (cdr lista_fecha)))))
          #t
          #f)
      #f))

(define (ID_r? entero_ID)
  (if (integer? entero_ID)
      #t
      #f))

(define (correlativo_r? entero_correlativo)
  (if (integer? entero_correlativo)
      #t
      #f))

(define (votos_r? lista_votos)
  (if (and (not(vacio? lista_votos)) (= (len lista_votos 0) 2))
      (if (and (integer? (car lista_votos)) (integer? (car (cdr lista_votos))))
          #t
          #f)
      #f))

(define (estado_r? entero_estado)
  (if (and (integer? entero_estado) (or (= entero_estado 0) (= entero_estado 1)))
      #t
      #f))

(define (reportes_r? entero_reportes)
  (if (integer? entero_reportes)
      #t
      #f))

(define (respuesta? lista_respuesta)
  (if (and (not (vacio? lista_respuesta)) (= (len lista_respuesta 0) 8))
      (if (and (nombre_r? (getNombre_r lista_respuesta)) (fecha_r? (getFecha_r lista_respuesta)) (ID_r? (getID_r lista_respuesta)) (correlativo_r? (getCorrelativo_r lista_respuesta)) (votos_r? (getVotos_r lista_respuesta)) (estado_r? (getEstado_r lista_respuesta)) (reportes_r? (getReportes_r lista_respuesta)))
          #t
          #f)
      #f))

(define (respuestas? lista_respuestas)
  (if (vacio? lista_respuestas)
      #t
      (if (respuesta? (car lista_respuestas))
          (respuestas? (cdr lista_respuestas))
          #f)))


; --------------------------------------------------------------------------------------------------------

(provide respuesta?)
(provide respuestas?)
(provide getRespuestas_usuario)
(provide getRespuestas_pregunta)

; --------------------------------------------------------------------------------------------------------

;(define respuesta_1(list "Usuario1" (list 1 1 1111) 111 1 (list 1 1) 1 1 "Respuesta 1.1"))
;(define respuesta_2(list "Usuario1" (list 1 1 1112) 112 1 (list 1 9) 1 2 "Respuesta 2.0"))
;(define respuesta_3(list "Usuario2" (list 2 2 2222) 111 2 (list 2 2) 0 3 "Respuesta 1.2"))
;(define respuesta_4(list "Usuario1" (list 3 3 3333) 333 1 (list 3 3) 1 4 "Respuesta 3.0"))
;(define respuesta_5(list "Usuario4" (list 4 4 4444) 111 3 (list 4 4) 0 5 "Respuesta 1.3"))

;(define listaR(list respuesta_1 respuesta_2 respuesta_3 respuesta_4 respuesta_5))

;(getNombre_r usuario_1)
;(getFecha_r usuario_1)
;(getID_r usuario_1)
;(getCorrelativo_r usuario_1)
;(getVotos_r usuario_1)
;(getEstado_r usuario_1)
;(getReportes_r usuario_1)

;nombre_r? fecha_r? ID_r? correlativo_r? votos_r? estado_r? reportes_r?
;(nombre_r? (getNombre_r usuario_1))
;(fecha_r? (getFecha_r usuario_1))
;(ID_r? (getID_r usuario_1))
;(correlativo_r? (getCorrelativo_r usuario_1))
;(votos_r? (getVotos_r usuario_1))
;(estado_r? (getEstado_r usuario_1))
;(reportes_r? (getReportes_r usuario_1))

;(respuesta? usuario_1)
;(respuesta? usuario_2)
;(respuesta? usuario_3)
;(respuesta? usuario_4)
;(respuestas? listaR)

;(nombre_r? (getNombre_r usuario_2))
;(fecha_r? (getFecha_r usuario_2))
;(ID_r? (getID_r usuario_2))
;(correlativo_r? (getCorrelativo_r usuario_2))
;(votos_r? (getVotos_r usuario_2))
;(estado_r? (getEstado_r usuario_2))
;(reportes_r? (getReportes_r usuario_2))

;(getRespuestas_usuario "Usuario1" listaR (list ))
;(getRespuestas_pregunta 111 listaR (list ))





