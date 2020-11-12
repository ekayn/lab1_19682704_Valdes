#lang racket
; TDA_Usuarios
; ------------------------------- Preguntas -------------------------------

; ----------- Constructores -----------

; preguntas = lista(pregunta) ------ Definición: lista que contiene las preguntas echas por todos los usuarios registrados

; pregunta = lista(lista(votos) lista(respuestas) entero_ID lista(etiquetas) string(titulo) string(contenido) lista(fecha) lista(ultima_actividad) string(autor) entero_estado entero_recompensa entero_reporte)  ------ Definición: lista que contiene toda la informacion relevante para cada pregunta publicada

      ; lista(votos) = list(voto_1 voto_2) ------ Definición: lista que contiene los votos a favor(voto_1) y los votos en contra(voto_2) de una pregunta
      ; respuesta = lista()
      ; entero_ID = entero ------ Definición: entero que señala la dirección ID de la pregunta, cada pregunta tiene un ID diferente que va creciendo a medida que se van publicando preguntas
      ; etiqueta = ------ Definición: string que señala una etiqueta para la pregunta, ej: "java" "C++" "tipeo"
      ; string(titulo) = string ------ Definición: string que señala el titulo de la pregunta subida por el usuario
      ; string(contenido) = string ------ Definición: string que describe la pregunta en cuestión poniendo en contexto a los usuarios que quieran responder
      ; lista(fecha) = lista(dia mes año) ------ Definición: lista que contiene tres enteros en donde se señala la fecha en la que se publicó la pregunta
      ; lista(ultima actividad) = lista(respuesta modificacion fecha) ------ Definición: lista que contiene la ultima actividad echa por los usuarios, contiene tanto respuestas, modificaciones y fecha
      ; string(autor) = string ------ Definición: string que contiene el nombre de usuario que subio la pregunta
      ; entero_estado = entero ------ Definición: entero que señala si la pregunta esta abierta o cerrada, se señala como 0 si la pregunta esta cerrada y 1 si está abierta
      ; entero_recompensa = entero ------ Definición: entero que señala la recompensa que se dara al usuario que de la mejor respuesta
      ; entero_reporte = entero ------ Definición: entero contador que señala los reportes de spam u ofensivo

; ----------- Selectores -----------

; Para votos a favor:
(define (getVotos preguntas)(car (car preguntas)))
;(define (getFavor preguntas)(car (car (car preguntas))))
;(define (getContra preguntas)(car (cdr (car (car preguntas)))))

; Para respuesta:
(define (getRespuestas preguntas)(car (cdr (car preguntas))))

; Para ID:
(define (getID preguntas)(car (cdr (cdr (car preguntas)))))

; Para etiqueta:
(define (getEtiquetas preguntas)(car (cdr (cdr (cdr (car preguntas))))))

; Pata titulo:
(define (getTitulo preguntas)(car (cdr (cdr (cdr (cdr (car preguntas)))))))

; Para contenido:
(define (getContenido preguntas)(car (cdr (cdr (cdr (cdr (cdr (car preguntas))))))))

; Para fecha:
(define (getFecha preguntas)(car (cdr (cdr (cdr (cdr (cdr (cdr (car preguntas)))))))))

; Para ultima actividad:
(define (getUltimaAC preguntas)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (car preguntas))))))))))

; Para autor:
(define (getAutor preguntas)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (car preguntas)))))))))))

; Para estado:
(define (getEstado preguntas)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (car preguntas))))))))))))

; Para recompensa:
(define (getRecompensa preguntas)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (car preguntas)))))))))))))

; Para reporte:
(define (getReporte preguntas)(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (car preguntas))))))))))))))


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define pregunta1(list (list 1 0) (list "Respuesta1.1" "Respuesta1.2" "Respuesta1.3") 1 (list "java" "C++" "tipeo" "1") "Problemas con lab 2.1" "No se que cajaro hacer 1" (list 1 6 2020) (list "Ultima actividad 1" "Cambio echo 1" (list 12 11 2020)) "Usuario1" 1 20000 0))
(define pregunta2(list (list 2 3) (list "Respuesta2.1" "Respuesta2.2" "Respuesta2.3") 2 (list "java" "C++" "tipeo" "2") "Problemas con lab 2.2" "No se que cajaro hacer 2" (list 2 6 2020) (list "Ultima actividad 2" "Cambio echo 2" (list 12 11 2020)) "Usuario2" 1 30000 1))
(define listaP(list pregunta1 pregunta2))