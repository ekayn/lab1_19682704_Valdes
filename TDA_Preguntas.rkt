#lang racket
(require "Globales.rkt")
(require "TDA_Respuestas.rkt")

; ------------------------------- Preguntas -------------------------------


; ----------- Representación -----------


; preguntas = lista(pregunta) ------ Definición: lista que contiene las preguntas echas por todos los usuarios registrados

; pregunta = lista(lista(votos) lista(respuestas) entero_ID lista(etiquetas) string(titulo) string(contenido) lista(fecha) string(autor) entero_estado entero_recompensa entero_reporte lista(recompensas_retenidas))  ------ Definición: lista que contiene toda la informacion relevante para cada pregunta publicada

      ; lista(votos) = list(voto_1 voto_2) ------ Definición: lista que contiene los votos a favor(voto_1) y los votos en contra(voto_2) de una pregunta
      ; lista(respuestas) = listado de respuestas ------ Definición: lista que contiene todas las respuestas echas por los usuarios referente a la pregunta
      ; entero_ID = entero ------ Definición: entero que señala la dirección ID de la pregunta, cada pregunta tiene un ID diferente que va creciendo a medida que se van publicando preguntas
      ; lista(etiquetas) = listado de etiquetas ------ Definición: lista de strings que contiene las etiquetas que describen la pregunta. Ej: "java" "C++" "tipeo"
      ; string(titulo) = string ------ Definición: string que señala el titulo de la pregunta subida por el usuario
      ; string(contenido) = string ------ Definición: string que describe la pregunta en cuestión poniendo en contexto a los usuarios que quieran responder
      ; lista(fecha) = lista(dia mes año) ------ Definición: lista que contiene tres enteros en donde se señala la fecha en la que se publicó la pregunta
      ; string(autor) = string ------ Definición: string que contiene el nombre de usuario que subio la pregunta
      ; entero_estado = entero ------ Definición: entero que señala si la pregunta esta abierta o cerrada, se señala como 0 si la pregunta esta cerrada y 1 si está abierta
      ; entero_recompensa = entero ------ Definición: entero que señala la recompensa que se dara al usuario que de la mejor respuesta
      ; entero_reporte = entero ------ Definición: entero contador que señala los reportes de spam u ofensivo
      ; lista(recompensas_retenidas) = lista de recompensas retenidas ------ Definición: lista de recompensas retenidas echas por los usuarios a la pregunta, cada recompensa es de la forma lista(usuario recompensa)


; ----------- Constructores -----------


(define pregunta1(list (list 1 0) (getRespuestas_pregunta 1 listaR (list )) 1 (list "java" "C++" "tipeo" "1") "Titulo 1" "Contenido 1" (list 1 6 2020) "Usuario1" 1 20000 0 (list )))
(define pregunta2(list (list 2 3) (getRespuestas_pregunta 2 listaR (list )) 2 (list "java" "C++" "tipeo" "2") "Titulo 2" "Contenido 2" (list 2 6 2020) "Usuario2" 1 30000 1 (list )))
(define pregunta3(list (list 2 1) (getRespuestas_pregunta 3 listaR (list )) 3 (list "java" "C++" "tipeo" "3") "Titulo 3" "Contenido 3" (list 2 6 2020) "Usuario1" 1 30000 5 (list )))
(define pregunta4(list (list 1 0) (getRespuestas_pregunta 4 listaR (list )) 4 (list "java" "C++" "tipeo" "4") "Titulo 4" "Contenido 4" (list 2 6 2020) "Usuario2" 1 30000 0 (list )))
(define pregunta5(list (list 0 0) (getRespuestas_pregunta 5 listaR (list )) 5 (list "java" "C++" "tipeo" "5") "Titulo 5" "Contenido 5" (list 2 6 2020) "Usuario2" 1 30000 3 (list )))
(define pregunta6(list (list 9 7) (getRespuestas_pregunta 6 listaR (list )) 6 (list "java" "C++" "tipeo" "6") "Titulo 6" "Contenido 6" (list 2 6 2020) "Usuario3" 1 30000 2 (list )))

(define listaP(list pregunta1 pregunta2 pregunta3 pregunta4 pregunta5 pregunta6))


; ----------- Selectores -----------


(define (getVotos_p pregunta)(car pregunta)) ; Definicion: funcion que entrega los votos a favor y en contra de una pregunta -- Dominio: lista con formato pregunta – Recorrido: lista de enteros
(define (getRespuestas_p pregunta)(car (cdr pregunta))) ; Definicion: funcion que entrega todas las respuestas publicadas en la pregunta -- Dominio: lista con formato pregunta – Recorrido: lista de respuestas
(define (getID_p pregunta)(car (cdr (cdr pregunta)))) ; Definicion: funcion que entrega el entero ID que representa la pregunta -- Dominio: lista con formato pregunta – Recorrido: entero ID
(define (getEtiquetas_p pregunta)(car (cdr (cdr (cdr pregunta))))) ; Definicion: funcion que entrega la lista con las etiquetas de la pregunta -- Dominio: lista con formato pregunta – Recorrido: lista de strings
(define (getTitulo_p pregunta)(car (cdr (cdr (cdr (cdr pregunta)))))) ; Definicion: funcion que entrega el titulo de la pregunta -- Dominio: lista con formato pregunta – Recorrido: string
(define (getContenido_p pregunta)(car (cdr (cdr (cdr (cdr (cdr pregunta))))))) ; Definicion: funcion que entrega el contenido de la pregunta -- Dominio: lista con formato pregunta – Recorrido: string
(define (getFecha_p pregunta)(car (cdr (cdr (cdr (cdr (cdr (cdr pregunta)))))))) ; Definicion: funcion que entrega la fecha en la que se publico la pregunta -- Dominio: lista con formato pregunta – Recorrido: lista de enteros
(define (getAutor_p pregunta)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr pregunta))))))))) ; Definicion: funcion que entrega el usuario que publico la pregunta -- Dominio: lista con formato pregunta – Recorrido: string
(define (getEstado_p pregunta)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr pregunta)))))))))) ; Definicion: funcion que entrega el estado de la pregunta -- Dominio: lista con formato pregunta – Recorrido: entero
(define (getRecompensa_p pregunta)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr pregunta))))))))))) ; Definicion: funcion que entrega la recompensa que se le da a la mejor respuesta -- Dominio: lista con formato pregunta – Recorrido: entero
(define (getReporte_p pregunta)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr pregunta)))))))))))) ; Definicion: funcion que entrega la cantidad de reportes de spam u ofensivo que tiene la pregunta -- Dominio: lista con formato pregunta – Recorrido: entero
(define (getRecompensasR_p pregunta)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr pregunta))))))))))))) ; Definicion: funcion que entrega todas las recompensas retenidas de la pregunta -- Dominio: lista con formato pregunta – Recorrido: lista de recompensas retenidas


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


; Selectores específicos:
(define (getPreguntas_usuario usuario preguntas lista) ; Definicion: funcion que entrega todas las preguntas echas por el usuario ingresado -- Dominio: string, lista de preguntas y lista vacia – Recorrido: lista de preguntas
  (if (vacio? preguntas)
      (reversed lista (list ))
      (if (equal? usuario (getAutor_p (car preguntas)))
          (getPreguntas_usuario usuario (cdr preguntas) (cons (car preguntas) lista))
          (getPreguntas_usuario usuario (cdr preguntas) lista))))
  
(define (getPregunta_ID ID preguntas) ; Definicion: funcion que entrega una lista en especifico señalada con su ID -- Dominio: entero ID y lista de preguntas – Recorrido: lista con formato pregunta
  (if (vacio? preguntas)
      "ERROR CRITICO"
      (if (= ID (getID_p (car preguntas)))
          (car preguntas)
          (getPregunta_ID ID (cdr preguntas)))))


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


; ----------- Pertenencias -----------


(define (votos_p? lista_votos) ; Definicion: funcion que valida que el dato ingresado sea una lista con dos enteros -- Dominio: lista con dos enteros – Recorrido: true o false
  (if (and (not(vacio? lista_votos)) (= 2 (len lista_votos 0)))
      (if (and (integer? (car lista_votos)) (integer? (car (cdr lista_votos))))
          #t
          #f)
      #f))


(define (ID_p? entero_ID) ; Definicion: funcion que valida que el dato ingresado sea un entero -- Dominio: entero – Recorrido: true o false
  (if (integer? entero_ID)
      #t
      #f))


(define (etiquetas_p? lista_etiquetas) ; Definicion: funcion que valida que el dato ingresado sea una lista de strings -- Dominio: lista de string – Recorrido: true o false
  (if (vacio? lista_etiquetas)
      #t
      (if (string? (car lista_etiquetas))
          (etiquetas_p? (cdr lista_etiquetas))
          #f)))


(define (titulo_p? string_titulo) ; Definicion: funcion que valida que el dato ingresado sea un string -- Dominio: string – Recorrido: true o false
  (if (string? string_titulo)
      #t
      #f))


(define (contenido_p? string_contenido) ; Definicion: funcion que valida que el dato ingresado sea un string -- Dominio: string – Recorrido: true o false
  (if (string? string_contenido)
      #t
      #f))


(define (fecha_p? lista_fecha) ; Definicion: funcion que valida que el dato ingresado sea una lista con tres enteros -- Dominio: lista con tres enteros – Recorrido: true o false
  (if (and (not(vacio? lista_fecha)) (= (len lista_fecha 0) 3))
      (if (and (integer? (car lista_fecha)) (integer? (car (cdr lista_fecha))) (integer? (car (cdr (cdr lista_fecha)))))
          #t
          #f)
      #f))


(define (autor_p? string_autor) ; Definicion: funcion que valida que el dato ingresado sea un string -- Dominio: string – Recorrido: true o false
  (if (string? string_autor)
      #t
      #f))


(define (estado_p? entero_estado) ; Definicion: funcion que valida que el dato ingresado sea un entero -- Dominio: entero – Recorrido: true o false
  (if (integer? entero_estado)
      #t
      #f))


(define (recompensa_p? entero_recompensa) ; Definicion: funcion que valida que el dato ingresado sea un entero -- Dominio: entero – Recorrido: true o false
  (if (integer? entero_recompensa)
      #t
      #f))


(define (reporte_p? entero_reporte) ; Definicion: funcion que valida que el dato ingresado sea un entero -- Dominio: entero – Recorrido: true o false
  (if (integer? entero_reporte)
      #t
      #f))


(define (recompensaR_p? lista_recompensasR) ; Definicion: funcion que valida que el dato ingresado sea una lista con recompensas retenidas -- Dominio: lista con recompensas retenidas – Recorrido: true o false
      (if (vacio? lista_recompensasR)
          #t
          (if (and (string? (car (car lista_recompensasR))) (integer? (car (cdr (car lista_recompensasR)))))
              (recompensaR_p? (cdr lista_recompensasR))
              #f)))          


(define (pregunta? lista_pregunta) ; Definicion: funcion que valida que el dato ingresado sea pregunta con todos sus elementos correspondientes -- Dominio: lista con formato pregunta  – Recorrido: true o false
  (if (and (not(vacio? lista_pregunta)) (= (len lista_pregunta 0) 12))
      (if (and (votos_p? (getVotos_p lista_pregunta)) (respuestas? (getRespuestas_p lista_pregunta)) (ID_p? (getID_p lista_pregunta)) (etiquetas_p? (getEtiquetas_p lista_pregunta)) (titulo_p? (getTitulo_p lista_pregunta)) (contenido_p? (getContenido_p lista_pregunta)) (fecha_p? (getFecha_p lista_pregunta)) (autor_p? (getAutor_p lista_pregunta)) (estado_p? (getEstado_p lista_pregunta)) (recompensa_p? (getRecompensa_p lista_pregunta)) (reporte_p? (getReporte_p lista_pregunta)) (recompensaR_p? (getRecompensasR_p lista_pregunta)))
          #t
          #f)
      #f))


(define (preguntas? lista_preguntas) ; Definicion: funcion que valida que el dato ingresado sea una lista de preguntas -- Dominio: lista de preguntas  – Recorrido: true o false
  (if (vacio? lista_preguntas)
      #t
      (if (pregunta? (car lista_preguntas))
          (preguntas? (cdr lista_preguntas))
          #f)))


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


(provide pregunta?)
(provide preguntas?)
(provide getPreguntas_usuario)
(provide getPregunta_ID)

(provide getVotos_p)
(provide getRespuestas_p)
(provide getID_p)
(provide getEtiquetas_p)
(provide getTitulo_p)
(provide getContenido_p)
(provide getFecha_p)
(provide getAutor_p)
(provide getEstado_p)
(provide getRecompensa_p)
(provide getReporte_p)
(provide getRecompensasR_p)

(provide listaP)


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

