#lang racket

; 1. insert
(define insert
  (lambda (n lst)
    (cond
      [(null? lst) (my-append lst (cons n '()))]
      [else (quicksort (my-append lst (cons n '())))]
    )
  )
)

; 2. insertion-sort
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

; ------------------------------------------------------------
; Partes de código que nos pueden ayudar mucho, sesión 1 y 2

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