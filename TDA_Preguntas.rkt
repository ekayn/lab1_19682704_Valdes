#lang racket
(require "Globales.rkt")
(require "TDA_Respuestas.rkt")

; ------------------------------- Preguntas -------------------------------


; ----------- Constructores -----------


; preguntas = lista(pregunta) ------ Definición: lista que contiene las preguntas echas por todos los usuarios registrados

; pregunta = lista(lista(votos) lista(respuestas) entero_ID lista(etiquetas) string(titulo) string(contenido) lista(fecha) lista(ultima_actividad) string(autor) entero_estado entero_recompensa entero_reporte)  ------ Definición: lista que contiene toda la informacion relevante para cada pregunta publicada

      ; lista(votos) = list(voto_1 voto_2) ------ Definición: lista que contiene los votos a favor(voto_1) y los votos en contra(voto_2) de una pregunta
      ; lista(respuestas) = listado de respuestas ------ Definición: lista que contiene todas las respuestas echas por los usuarios referente a la pregunta
      ; entero_ID = entero ------ Definición: entero que señala la dirección ID de la pregunta, cada pregunta tiene un ID diferente que va creciendo a medida que se van publicando preguntas
      ; lista(etiquetas) = listado de etiquetas ------ Definición: lista de strings que contiene las etiquetas que describen la pregunta. Ej: "java" "C++" "tipeo"
      ; string(titulo) = string ------ Definición: string que señala el titulo de la pregunta subida por el usuario
      ; string(contenido) = string ------ Definición: string que describe la pregunta en cuestión poniendo en contexto a los usuarios que quieran responder
      ; lista(fecha) = lista(dia mes año) ------ Definición: lista que contiene tres enteros en donde se señala la fecha en la que se publicó la pregunta
      ; lista(ultima actividad) = lista(respuesta modificacion fecha) ------ Definición: lista que contiene la ultima actividad echa por los usuarios, contiene tanto respuestas, modificaciones y fecha
      ; string(autor) = string ------ Definición: string que contiene el nombre de usuario que subio la pregunta
      ; entero_estado = entero ------ Definición: entero que señala si la pregunta esta abierta o cerrada, se señala como 0 si la pregunta esta cerrada y 1 si está abierta
      ; entero_recompensa = entero ------ Definición: entero que señala la recompensa que se dara al usuario que de la mejor respuesta
      ; entero_reporte = entero ------ Definición: entero contador que señala los reportes de spam u ofensivo


; ----------- Selectores -----------


(define (getVotos_p pregunta)(car pregunta))
(define (getRespuestas_p pregunta)(car (cdr pregunta)))
(define (getID_p pregunta)(car (cdr (cdr pregunta))))
(define (getEtiquetas_p pregunta)(car (cdr (cdr (cdr pregunta)))))
(define (getTitulo_p pregunta)(car (cdr (cdr (cdr (cdr pregunta))))))
(define (getContenido_p pregunta)(car (cdr (cdr (cdr (cdr (cdr pregunta)))))))
(define (getFecha_p pregunta)(car (cdr (cdr (cdr (cdr (cdr (cdr pregunta))))))))
(define (getUltimaAC_p pregunta)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr pregunta)))))))))
(define (getAutor_p pregunta)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr pregunta))))))))))
(define (getEstado_p pregunta)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr pregunta)))))))))))
(define (getRecompensa_p pregunta)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr pregunta))))))))))))
(define (getReporte_p pregunta)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr pregunta)))))))))))))


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


; Selectores específicos:
(define (getPreguntas_usuario usuario preguntas lista)
  (if (vacio? preguntas)
      (reversed lista (list ))
      (if (equal? usuario (getAutor_p (car preguntas)))
          (getPreguntas_usuario usuario (cdr preguntas) (cons (car preguntas) lista))
          (getPreguntas_usuario usuario (cdr preguntas) lista))))
  
(define (getPregunta_ID ID preguntas)
  (if (vacio? preguntas)
      "ERROR CRITICO"
      (if (= ID (getID_p (car preguntas)))
          (car preguntas)
          (getPregunta_ID ID (cdr preguntas)))))


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


; ----------- Pertenencias -----------


(define (votos_p? lista_votos)
  (if (and (not(vacio? lista_votos)) (= 2 (len lista_votos 0)))
      (if (and (integer? (car lista_votos)) (integer? (car (cdr lista_votos))))
          #t
          #f)
      #f))

(define (ID_p? entero_ID)
  (if (integer? entero_ID)
      #t
      #f))

(define (etiquetas_p? lista_etiquetas)
  (if (vacio? lista_etiquetas)
      #t
      (if (string? (car lista_etiquetas))
          (etiquetas_p? (cdr lista_etiquetas))
          #f)))

(define (titulo_p? string_titulo)
  (if (string? string_titulo)
      #t
      #f))

(define (contenido_p? string_contenido)
  (if (string? string_contenido)
      #t
      #f))

(define (fecha_p? lista_fecha)
  (if (and (not(vacio? lista_fecha)) (= (len lista_fecha 0) 3))
      (if (and (integer? (car lista_fecha)) (integer? (car (cdr lista_fecha))) (integer? (car (cdr (cdr lista_fecha)))))
          #t
          #f)
      #f))

(define (ultimaAC_p? lista_UAC)
  (if (and (not(vacio? lista_UAC)) (= (len lista_UAC 0) 3))
      (if (and (string? (car lista_UAC)) (string? (car (cdr lista_UAC))) (fecha_p? (car (cdr (cdr lista_UAC)))))
          #t
          #f)
      #f))

(define (autor_p? string_autor)
  (if (string? string_autor)
      #t
      #f))

(define (estado_p? entero_estado)
  (if (integer? entero_estado)
      #t
      #f))

(define (recompensa_p? entero_recompensa)
  (if (integer? entero_recompensa)
      #t
      #f))

(define (reporte_p? entero_reporte)
  (if (integer? entero_reporte)
      #t
      #f))

(define (pregunta? lista_pregunta)
  (if (and (not(vacio? lista_pregunta)) (= (len lista_pregunta 0) 12))
      (if (and (votos_p? (getVotos_p lista_pregunta)) (respuestas? (getRespuestas_p lista_pregunta)) (ID_p? (getID_p lista_pregunta)) (etiquetas_p? (getEtiquetas_p lista_pregunta)) (titulo_p? (getTitulo_p lista_pregunta)) (contenido_p? (getContenido_p lista_pregunta)) (fecha_p? (getFecha_p lista_pregunta)) (ultimaAC_p? (getUltimaAC_p lista_pregunta)) (autor_p? (getAutor_p lista_pregunta)) (estado_p? (getEstado_p lista_pregunta)) (recompensa_p? (getRecompensa_p lista_pregunta)) (reporte_p? (getReporte_p lista_pregunta)))
          #t
          #f)
      #f))

(define (preguntas? lista_preguntas)
  (if (vacio? lista_preguntas)
      #t
      (if (pregunta? (car lista_preguntas))
          (preguntas? (cdr lista_preguntas))
          #f)))



; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


(define pregunta1(list (list 1 0) (getRespuestas_pregunta 1 listaR (list )) 1 (list "java" "C++" "tipeo" "1") "Titulo 1" "Contenido 1" (list 1 6 2020) (list "Ultima actividad 1" "Cambio 1" (list 12 11 2020)) "Usuario1" 1 20000 0))
(define pregunta2(list (list 2 3) (getRespuestas_pregunta 2 listaR (list )) 2 (list "java" "C++" "tipeo" "2") "Titulo 2" "Contenido 2" (list 2 6 2020) (list "Ultima actividad 2" "Cambio 2" (list 12 11 2020)) "Usuario2" 1 30000 1))
(define pregunta3(list (list 2 1) (getRespuestas_pregunta 3 listaR (list )) 3 (list "java" "C++" "tipeo" "3") "Titulo 3" "Contenido 3" (list 2 6 2020) (list "Ultima actividad 3" "Cambio 3" (list 12 11 2020)) "Usuario1" 1 30000 5))
(define pregunta4(list (list 1 0) (getRespuestas_pregunta 4 listaR (list )) 4 (list "java" "C++" "tipeo" "4") "Titulo 4" "Contenido 4" (list 2 6 2020) (list "Ultima actividad 4" "Cambio 4" (list 12 11 2020)) "Usuario2" 1 30000 0))
(define pregunta5(list (list 0 0) (getRespuestas_pregunta 5 listaR (list )) 5 (list "java" "C++" "tipeo" "5") "Titulo 5" "Contenido 5" (list 2 6 2020) (list "Ultima actividad 5" "Cambio 5" (list 12 11 2020)) "Usuario2" 1 30000 3))
(define pregunta6(list (list 9 7) (getRespuestas_pregunta 6 listaR (list )) 6 (list "java" "C++" "tipeo" "6") "Titulo 6" "Contenido 6" (list 2 6 2020) (list "Ultima actividad 6" "Cambio 6" (list 12 11 2020)) "Usuario3" 1 30000 2))

(define listaP(list pregunta1 pregunta2 pregunta3 pregunta4 pregunta5 pregunta6))


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


(provide pregunta?)
(provide preguntas?)
(provide getPreguntas_usuario)
(provide getPregunta_ID)

(provide listaP)


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

