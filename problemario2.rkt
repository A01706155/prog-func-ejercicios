#lang racket

;; 1. insert
;recibe un numero y una lista que contiene n numeros en orden ascendente
;devuelve una nueva lista con los mismos elementos delstpero conninsertado en su lugar correspondiente
(define my-append
  (lambda (lst1 lst2)
    (cond
      [(null? lst1) lst2]
      [else (cons (car lst1)
                  (my-append (cdr lst1) lst2))])))

(define in-lst
  (lambda (elem)
    (cons elem '())))

(define insert
  (lambda (n lst)
    (cond
      [(null? lst) (my-append lst (in-lst n))]
      [else (insertion-sort (my-append lst (in-lst n)))])))

;; 2. insertion-sort
;recibe una lista desordenada y devuelve la misma lista ordenada de manera ascendente
(define insertion
  (lambda (n lst)
    (if (null? lst)
        (list n)
        (let ((x (car lst))
              (xs (cdr lst)))
          (if (<= n x)
              (cons n lst)
              (cons x (insertion n xs)))))))

(define insertion-sort
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [else (insertion (car lst)
                     (insertion-sort (cdr lst)))])))
;; 3. rotate-left
;recibe un numero n y una lista lst, devuelve la lista que resulta de rotar lst un total de n elementos a la izquierda
(define reverse
  (lambda (list)
    (cond
      [(null? list) '()]
      [else (my-append (reverse (cdr list))
                       (cons (car list) '()))])))

(define rotate-left
  (lambda (n lst)
    (cond
      [(null? lst) '()]
      [(eq? n 0) lst]
      [(< n 0) (rotate-left (+ n 1) (my-append (in-lst (car (reverse lst))) (reverse (cdr (reverse lst)))))]
      [else (rotate-left (- n 1) (my-append (cdr lst) (in-lst (car lst))))])))

;; 7. insert-anywhere
;recibe un objeto y una lista, devuelve la lita con el objeto insertado en todas las posiciones posibles
(define reverse2
  (lambda (list acc)
    (cond
      [(empty? list) acc]
      [else (reverse2 (cdr list) (cons (car list) acc))])))

(define aux
  (lambda (x lst lst2 len)
    (if (eq? len -1) lst2
    (aux x lst (cons (index x lst '() len) lst2) (- len 1)))))

(define index
  (lambda (x lst new ind)
    (if (eq? ind 0) (append (reverse2 (cons x new) '()) lst)
        (index x (cdr lst) (cons (car lst) new) (- ind 1)))))

(define insert-everywhere
  (lambda (x lst)
    (aux x lst '() (length lst)))
  )

;; 8 pack
;recibe una lista y devuelve una lista de listas que agrupan los elementos de la primera lista



;; 9. compress
;recibe una lista y devuelve otra lista en la cual los elementos repetidos de la primera lista se reemplazan por una sola insatncia
(define compress
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [(< (length lst) 2) (my-append (in-lst (car lst)) '())]
      [(eq? (car lst) (cadr lst)) (compress (cdr lst))]
      [else (my-append (in-lst (car lst)) (compress (cdr lst)))])))

;; 12. decode
;recibe una lista codificada y la devuelve descodificada
(define repeat-head
  (lambda (atom amount)
    (cond
      [(eq? amount 0) '()]
      [else (cons atom (repeat-head atom (- amount 1)))])))

(define mult
  (lambda (elem)
    (cond
      [(pair? elem) (repeat-head (cadr elem) (car elem))]
      [else (in-lst elem)])))

(define decode
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [else (my-append (mult (car lst))
                       (decode (cdr lst)))])))
