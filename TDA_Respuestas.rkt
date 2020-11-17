#lang racket
(require "Globales.rkt")


; ------------------------------- Respuestas -------------------------------


; ----------- Constructores -----------


; respuestas = lista(respuesta) ------ Definición: lista que contiene todas las respuestas echas por los usuarios registrados

; respuesta = lista(string(usuario) lista(fecha) entero_ID entero_correlativo lista(votos) entero_estado entero_reportes string(respuesta) entero_recompensa)
      ; string(usuario) = string ------ Definición: string que señala el nombre del usuario que respondio
      ; lista(fecha) = lista(dia mes ano) ------ Definición: lista de enteros que contiene la fecha en la que se registro la respuesta
      ; entero_ID = entero ------ Definición: entero que registra la direccion ID de la pregunta que se respondio
      ; entero_correlativo = entero ------ Definición: entero que señala la posicion de registro de la respuesta en la pregunta
      ; lista(votos) = lista(voto1 voto2) ------ Definición: lista que contiene la cantidad de votos positivos(voto1) y la cantidad de votos negativos(voto2)
      ; entero_estado = entero ------ Definición: entero que señala si la respuesta es aseptada(1) o rechazada(0)
      ; entero_reportes = entero ------ Definición: entero que señala la cantidad de reportes de spam u ofensa
      ; string(respuesta) = string ------ Definición: string que señala la respuesta publicada por el usuario
      ; entero_recompensa = entero ------ Definición: entero que señala si una respuesta recive recompensa, toma directamente el valor de la recompensa si es dada


; ----------- Selectores -----------


(define (getNombre_r respuesta)(car respuesta))
(define (getFecha_r respuesta)(car (cdr respuesta)))
(define (getID_r respuesta)(car (cdr (cdr respuesta))))
(define (getCorrelativo_r respuesta)(car (cdr (cdr (cdr respuesta)))))
(define (getVotos_r respuesta)(car (cdr (cdr (cdr (cdr respuesta))))))
(define (getEstado_r respuesta)(car (cdr (cdr (cdr (cdr (cdr respuesta)))))))
(define (getReportes_r respuesta)(car (cdr (cdr (cdr (cdr (cdr (cdr respuesta))))))))
(define (getRespuesta_r respuesta)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr respuesta)))))))))
(define (getRecompensa_r respuesta)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr respuesta))))))))))


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


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


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


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


(define (respuesta_r? string_respuesta)
  (if (string? string_respuesta)
      #t
      #f))

(define (recompensa_r? entero_recompensa)
  (if (integer? entero_recompensa)
      #t
      #f))


(define (respuesta? lista_respuesta)
  (if (and (not (vacio? lista_respuesta)) (= (len lista_respuesta 0) 10))
      (if (and (nombre_r? (getNombre_r lista_respuesta)) (fecha_r? (getFecha_r lista_respuesta)) (ID_r? (getID_r lista_respuesta)) (correlativo_r? (getCorrelativo_r lista_respuesta)) (votos_r? (getVotos_r lista_respuesta)) (estado_r? (getEstado_r lista_respuesta)) (reportes_r? (getReportes_r lista_respuesta)) (respuesta_r? (getRespuesta_r lista_respuesta)) (recompensa_r? (getRecompensa_r lista_respuesta)))
          #t
          #f)
      #f))

(define (respuestas? lista_respuestas)
  (if (vacio? lista_respuestas)
      #t
      (if (respuesta? (car lista_respuestas))
          (respuestas? (cdr lista_respuestas))
          #f)))


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


(define respuesta_1(list "Usuario1" (list 1 1 1111) 1 1 (list 1 1) 1 1 "Respuesta 1.1" 0))
(define respuesta_2(list "Usuario1" (list 1 1 1112) 2 1 (list 1 9) 1 2 "Respuesta 2.1" 0))
(define respuesta_3(list "Usuario2" (list 2 2 2222) 1 2 (list 2 2) 0 3 "Respuesta 1.2" 0))
(define respuesta_4(list "Usuario1" (list 3 3 3333) 3 1 (list 3 3) 1 4 "Respuesta 3.1" 0))
(define respuesta_5(list "Usuario4" (list 4 4 4444) 1 3 (list 4 4) 0 5 "Respuesta 1.3" 0))
(define respuesta_6(list "Usuario2" (list 3 3 3333) 1 4 (list 3 3) 1 4 "Respuesta 1.4" 0))
(define respuesta_7(list "Usuario4" (list 3 3 3333) 2 2 (list 3 3) 1 0 "Respuesta 2.2" 0))
(define respuesta_8(list "Usuario4" (list 3 3 3333) 2 3 (list 3 3) 1 1 "Respuesta 2.3" 0))
(define respuesta_9(list "Usuario4" (list 3 3 3333) 2 4 (list 3 3) 1 2 "Respuesta 2.4" 0))
(define respuesta_10(list "Usuario1" (list 3 3 3333) 3 2 (list 3 3) 1 3 "Respuesta 3.2" 0))
(define respuesta_11(list "Usuario2" (list 3 3 3333) 3 3 (list 3 3) 1 0 "Respuesta 3.3" 0))
(define respuesta_12(list "Usuario3" (list 3 3 3333) 3 4 (list 3 3) 1 2 "Respuesta 3.4" 0))
(define respuesta_13(list "Usuario4" (list 3 3 3333) 3 5 (list 3 3) 1 0 "Respuesta 3.5" 0))
(define respuesta_14(list "Usuario2" (list 3 3 3333) 4 1 (list 3 3) 1 4 "Respuesta 4.1" 0))
(define respuesta_15(list "Usuario5" (list 3 3 3333) 4 2 (list 3 3) 1 4 "Respuesta 4.2" 0))
(define respuesta_16(list "Usuario2" (list 3 3 3333) 5 1 (list 3 3) 1 4 "Respuesta 5.1" 0))
(define respuesta_17(list "Usuario4" (list 3 3 3333) 6 1 (list 3 3) 1 4 "Respuesta 6.1" 0))
(define respuesta_18(list "Usuario5" (list 3 3 3333) 5 2 (list 3 3) 1 4 "Respuesta 5.2" 0))
(define respuesta_19(list "Usuario5" (list 3 3 3333) 5 3 (list 3 3) 1 4 "Respuesta 5.3" 0))

(define listaR(list respuesta_1 respuesta_2 respuesta_3 respuesta_4 respuesta_5 respuesta_6 respuesta_7 respuesta_8 respuesta_9 respuesta_10 respuesta_11 respuesta_12 respuesta_13 respuesta_14 respuesta_15 respuesta_16 respuesta_17 respuesta_18 respuesta_19))


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


(provide respuesta?)
(provide respuestas?)
(provide getRespuestas_usuario)
(provide getRespuestas_pregunta)

(provide listaR)


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


