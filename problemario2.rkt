#lang racket

; #############################
; # Ejercicios Manolo Ramírez #
; #############################

;; 1. insert
(define insert
  (lambda (n lst)
    (cond
      [(null? lst) (my-append lst (cons n '()))]
      [else (quicksort (my-append lst (cons n '())))]
    )
  )
)

;; 2. insertion-sort
(define (ins num lst)
    (if (null? lst) (list num)
        
        (let ((x (car lst)) (xs (cdr lst))
        )
        
        (if (<= num x) (cons num lst) (cons x (ins num xs)))
        )
    )
)

(define (insertion-sort lst)
    (cond
      [(null? lst) '()]
      [else (ins (car lst)(insertion-sort (cdr lst)))]
    )
)

;; 3: rotate-left
(define (rotate-left n lst)
    (cond

      [(null? lst) '()]
      [(eq? n 0) lst]
      [(< n 0) (rotate-left (+ n 1)
                   (my-append (cons (car (reverse-head lst) '() ))
                   (reverse-head (cdr (reverse-head lst))))
               )
      ]
      [else (rotate-left (- n 1) (my-append (cdr lst) (cons (car lst) '())))]
      
    )
)

; ##########################
; # Ejercicios Daniel Cruz #
; ##########################

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
(define (pack lst)
 (cond [(null? lst) null]
       [else (group (cdr lst) (list (car lst)))]))
 
(define (group lst lst2)
  (define n -1)
 (cond [(and (pair? lst)
             (eq? (car lst) (list-ref lst2 (add1 n))))
        (group (cdr lst)
                        (cons (car lst) lst2))]
       [else (cons (reverse lst2) (pack lst))]))

;; 9. compress
;recibe una lista y devuelve otra lista en la cual los elementos repetidos de la primera lista se reemplazan por una sola insatncia
(define compress
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [(< (length lst) 2) (my-append (in-lst (car lst)) '())]
      [(eq? (car lst) (cadr lst)) (compress (cdr lst))]
      [else (my-append (in-lst (car lst)) (compress (cdr lst)))])))

;; 11. encode-modified
;recibe una lista y devuelve los elementos consecutivos codificados en listas de la forma (n e)
;si un elemento no tiene duplicados simplemente se copia en la lista resultante
(define (encode-modified lst)
    (cond
      [(empty? lst) lst]
      [(enmod lst '() 0 0)]))

(define (enmod lst new val cont)
    (if (empty? lst) (if (eq? cont 1)
        (reverse (cons val new) '()) (reverse2 (cons (cons cont (list val)) new) '()))
            (cond
              [(eq? cont 0) (enmod (cdr lst) new (car lst) 1)]
              [(and (not(eq? val (car lst))) (eq? cont 1)) (enmod (cdr lst) (cons val new) (car lst) 1)]
              [(not(eq? val (car lst))) (enmod (cdr lst) (cons (cons cont (list val)) new) (car lst) 1)]
              [else (enmod (cdr lst) new val (+ cont 1))])))

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

;; Ejercicio 14
(define (there-exists-one? pred lst)
        (cond
          [(null? lst) #f]
          [(pred (car lst))]
          [else (there-exists-one? pred (cdr lst))]))

; .------------------------------------------------------------.
; | Partes de código que nos pueden ayudar mucho, sesión 1 y 2 |
; .------------------------------------------------------------.
(define my-append
  (lambda (lst1 lst2)
    (cond
      [(null? lst1) lst2]
      [else (cons (car lst1)
                  (my-append (cdr lst1) lst2))])))
;; example: (my-append '(1 2) '(3 4)) => '(1 2 3 4)

(define quicksort
  (lambda (list)
    (cond
      [(null? list) '()]
      [else (append (quicksort (small-items list (car list)))
                    (cons (car list) '())
                    (quicksort (large-items list (car list))))])))
;; example: (quicksort '(71 73 26 82)) => '(26 71 73 82)

(define small-items
  (lambda (list threshold)
    (cond
      [(null? list) '()]
      [(< (car list) threshold) (cons (car list)
                                      (small-items (cdr list) threshold))]
      [else (small-items (cdr list) threshold)])))
;; example: (small-items '(71 73 26 82) 50) = '(26)

(define large-items
  (lambda (list threshold)
    (cond
      [(null? list) '()]
      [(> (car list) threshold) (cons (car list)
                                      (large-items (cdr list) threshold))]
      [else (large-items (cdr list) threshold)])))
;; example: (large-items '(71 73 26 82) 50) = '(71 73 82)

(define reverse-head
  (lambda (list)
    (cond
      [(null? list) '()]
      [else (my-append (reverse-head (cdr list))
                       (cons (car list) '()))])))
;; example: (reverse-head '(1 2 3)) => '(3 2 1)