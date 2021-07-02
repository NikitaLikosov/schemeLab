#lang scheme

;quotient
(define (mod10 x)(remainder x 10))

(define (div10 x)(/ (- x (mod10 x)) 10))

(define (div x y)(= (remainder x y) 0))
(define (memberCust x y) 
  (if (empty? y) #f (if (equal? x (car y)) y (memberCust x (cdr y)) ))
)

(define (minstr a) (foldl min +inf.0  a))
(define (maxstr a) (foldl max -inf.0 a))

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

(define (trans a) 
  (map (λ (n) (foldr (λ (y rez) (cons (list-ref y n) rez)) `() a)) (build-list (length (car a)) values))
)
(define (check-duplicates x) 
  (cond 
    ((empty? x) #f)
    ((member (car x) (cdr x)) (car x))
    (else (check-duplicates (cdr x))))
)
(define (char->dig chr)
  (cond
    [(equal? chr #\0) 0]
    [(equal? chr #\1) 1]
    [(equal? chr #\2) 2]
    [(equal? chr #\3) 3]
    [(equal? chr #\4) 4]
    [(equal? chr #\5) 5]
    [(equal? chr #\6) 6]
    [(equal? chr #\7) 7]
    [(equal? chr #\8) 8]
    [(equal? chr #\9) 9]))

;Начало решения задач лабораторной

;Пример:(Lab10_1 "42")
(define (Lab10_1 x) 
  (foldl (λ (x res) (+ (* res 10) (char->dig x))) 0 (string->list x))
)

;Пример:(Lab10_2 "123 123|123 " " |")
(define (Lab10_2 x y)
  
  (length (split x y))
)
;Пример:(split "123 123|123 " " |")
(define (split a b)
 (let ([a (string->list a)] [b (string->list b)])
   (foldr  (λ (x res1) (if (equal? x `()) res1 (cons (list->string x) res1))) `() 
           (foldr (λ (x res) (if (ormap (λ (n) (equal? n x)) b) 
                         (if (empty? (car res)) res (cons `() res))
                         (cons (cons x (car res)) 
                               (cdr res)))) `(()) a))
 )
) 

;Пример:(Lab10_4 "123 123|123 " " |")
(define (Lab10_4 x y)
 (check-duplicates (split x y))
)


(define (Lab10_4V2 a b)

 (let ([a (string->list a)] [b (string->list b)])
   (let  ([ z (check-duplicates
           (foldr (λ (x res) (if (ormap (λ (n) (equal? n x)) b) 
                         (if (empty? (car res)) res (cons `() res))
                         (cons (cons x (car res)) 
                               (cdr res)))) `(()) a))]) (if z (list->string z) z))
 
)
)