#lang racket


; ------------------------------- Funciones usadas como globales -------------------------------


(define (vacio? lista) ; Definicion: funcion que valida que la lista ingresada esta vacia -- Dominio: lista – Recorrido: true o false
  (if (null? lista)
      #t
      #f))

(define (len lista contador) ; Definicion: funcion que entrega la cantidad de elementos que tiene una lista -- Dominio: lista – Recorrido: entero
  (if (vacio? lista)
      contador
      (len (cdr lista) (+ contador 1))))

(define (reversed lista1 lista2) ; Definicion: funcion que entrega la lista ingresada con las posiciones de los elementos dados vuelta -- Dominio: lista – Recorrido: lista
  (if (null? lista1)
      lista2
      (reversed (cdr lista1) (cons (car lista1) lista2))))


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 

(provide vacio?)
(provide len)
(provide reversed)


; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 