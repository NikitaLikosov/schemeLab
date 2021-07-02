#lang scheme

;quotient
(define (mod10 x)(remainder x 10))

(define (div10 x)(/ (- x (mod10 x)) 10))

(define (div x y)(= (remainder x y) 0))
(define (memberCust x y) 
  (if (empty? y) #f (if (equal? x (car y)) y (memberCust x (cdr y)) ))
)



(define (++ x)(+ x 1))
(define (-- x)(- x 1))
(define (sim x)
  (define (simplN n)
    (if (< x (* n n)) #t (if (div x n) #f (simplN (+ n 1)))) 
  )
  (simplN 2)
)  
(define (NOD a b) 
  (let ([a (abs a)] [b (abs b)])
  (if (= 0 (* a b)) (+ a b) (NOD (min a b) (remainder (max a b) (min a b))) ))
)

;Начало решения задач лабораторной


;Пример: (Lab7_1 `(1 2 3 4 10 11))
(define (Lab7_1 a)  
  (filter (lambda (x) (div (car x) (cdr x))) (map (lambda (x y) (cons x y) ) a (build-list (length a) add1)))
)


;Пример: (Lab7_2 `( 3 4 10 11 123 321 1))
(define (Lab7_2 a)
  (foldl (lambda (x rez) (if (and (> (quotient x 100) 0)(< (quotient x 100) 10)) (+ x rez) rez)) 0 a)
)


;Пример: (Lab7_3 `(1 2 3) `(+ +))
(define (Lab7_3 li1 li2)
  (foldl (lambda (n func result) ((eval func) result n)) (car li1) (cdr li1) li2)
    
)
;Пример: (Lab7_4 `(1 2 2 4 5))
(define (Lab7_4 a)
  (foldr (lambda (n rez) (cons (cons n (if (empty? rez) `() (car rez))) rez)) `() a)
)

;Пример: (Lab7_5 1 0 `((1 + 10) (1 + 10) (15 - 10)))
(define (Lab7_5 a b c)
  (foldl 
   (lambda (z rez) (* (foldl (lambda (item x) ((eval (cadr item)) x (caddr item))) 
                             rez 
                             (filter (lambda (x) (= z (car x))) c)) (+ 1 (/ b 100)))) 
     a 
     (build-list 30 add1))

)
























