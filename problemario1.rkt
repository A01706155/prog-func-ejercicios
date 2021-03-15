#lang racket

; ##########################
; # Ejercicios Daniel Cruz #
; ##########################

; 1. fahrenheit-to-celsius 
;toma como entrada una temperaturafen grados Fahrenheit y laconvierte a su equivalente en grados Celsius 
(define (fahrenheit-to-celsius f)
  (/ (* 5 (- f 32)) 9) ;;formula
)

; 2. sign
;regresa 1 si el numero es positivo y -1 si es negativo
(define (sign n)
 (if (>= n 0) 1 -1) 
  )

; 3. roots
;devuelve la raiz que resuelve la ecuacion cuadratica
(define (roots a b c)
  (/ (+ (- b) (sqr (- (* b b) (* 4 a c)))) (* 2 a))
  )

; 4. bm1
;devuelve el indice de masa corporal a partir del peso y la altura
;tambien devuelve condiciones de peso basadas en el resultado
(define (bmi w h)
  (define imb (/ w (* h h)))
  (cond
    [(< imb 20) "underweight"]
    [(<= imb 25) "normal"]
    [(<= imb 30) "obese1"]
    [(<= imb 40) "obese2"]
    [(> imb 40) "obese3"]
    )
  )

; 5. factorial
;toma un entero positivo y devuelve su factorial
(define (factorial n)
  (cond
    [(< n 1) 1]
    [else (* n (factorial(- n 1)))])
  )

; 7. pow
;dados 2 numeros a y b devuelve el resultado de a elevado a la potencia b
(define (pow a b)
  (expt a b)
  )

; 16. average
;devuelve el promedio de una lista dada como parametro
(define (sum lst)
(if (null? lst) 0
    (+ (car lst) (sum (cdr lst))))
  )

(define (average lst)
  (if (empty? lst) 0
      (/ (sum lst) (length lst)))
  )
 
; #############################
; # Ejercicios Manolo Ramírez #
; #############################

; 6. duplicate
(define (duplicate lst)
  (cond
    [(null? lst) '()]
    [else (my-append (repeat-head (car lst) 2) (duplicate (cdr lst)))]
    )
)

; 8. fib
(define (fib n)
  (if (<= n 1)
      1 (+ (fib (- n 1)) (fib (- n 2)))
  )
)

;10 positives
(define (positives lst)
    (cond
      [(null? lst) '()]
      [(>= (car lst) 0) (my-append (cons(car lst) '()) (positives (cdr lst)))]
      [else (positives (cdr lst))]))

; 11. add-list
(define (add-list lst)
    (if (empty? lst) 0 (+ (car lst) 
        (add-list (cdr lst)))
    )
)

; 12. invert-pairs
(define (invert-pairs lst)
  (map (lambda (lst2) (reverse lst2)) lst
  )
)

;


;



; ------------------------------------------------------------
; Partes de código que nos pueden ayudar mucho, sesión 1 y 2
(define repeat-head
  (lambda (atom amount)
    (cond
      [(eq? amount 0) '()]
      [else (cons atom (repeat-head atom (- amount 1)))])))
;; example: (repeat-head 'a 4) => '(a a a a)

(define my-append
  (lambda (lst1 lst2)
    (cond
      [(null? lst1) lst2]
      [else (cons (car lst1)
                  (my-append (cdr lst1) lst2))])))
;; example: (my-append '(1 2) '(3 4)) => '(1 2 3 4)
; ------------------------------------------------------------
