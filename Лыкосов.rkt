#lang scheme

;quotient
(define (mod10 x)(remainder x 10))

(define (div10 x)(/ (- x (mod10 x)) 10))

(define (div x y)(= (remainder x y) 0))
(define (memberCust x y) 
  (if (empty? y) #f (if (equal? x (car y)) y (memberCust x (cdr y)) ))
  )
(define (sum-list li) (foldl + 0 li))
(define (composition-list li) (foldl * 1 li))
(define (isfigure n) (and (= (div10 n) 0) (>= n 0)))

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

(define (build-list-segment x y) 
  (build-list (+ (- y x) 1) (λ (el) (+ el x)))  
  )
(define (max-square n)
  (define (iter test)
    (if (> (* test test) (abs n)) (- test 1) (iter (++ test))) 
    )
  (iter 2)
  )
(define (** x n) 
  (if (= n 0) 1 (* x (** x (-- n))))
  )
(define (calc-pol A x)
  (sum-list (map * A (reverse (build-list (length A) (λ (n) (** x n))))))
  )

(define left cadr)
(define right caddr)
(define data car)
(define (isleaf? tree) 
  (and (empty? (left tree)) (empty? (right tree)))
  ) 


;Начало решения задач лабораторной
;Пример:(Lab17_1 ++ `(3 (2 (4 () ()) (4 () ())) (2 (3 () ()) (4 (3 () ()) ()))))

  




































  
  
  
  
  
  
  