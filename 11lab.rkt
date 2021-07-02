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


;Начало решения задач лабораторной

;Пример:(Lab11_1 1 100) c 1
(define (Lab11_1 a b)
  (define (test-od x max-sq n) 
    (and (<= n max-sq) (or (and (div x n) (div (/ x n) n)) (test-od x max-sq (++ n)))) 
  )
  (filter (λ (el) (if (= el 1) 1 (test-od el (max-square el) 2))) (build-list-segment a b))
)
; без 1
(define (Lab11_1V2 a b)
  (define (test-od x max-sq n) 
    (and (<= n max-sq) (or (and (div x n) (div (/ x n) n)) (test-od x max-sq (++ n)))) 
  )
  (filter (λ (el) (test-od el (max-square el) 2)) (build-list-segment a b))
)



;Пример:(Lab11_2 `{3 5 52 24 23 3 1 5}) или проще (Lab11_2 (build-list 35 values))
(define (Lab11_2 li)
  (define (iter x1 x2 li)
    (if (>=(length (cdr li)) (- x1 x2)) (cons (car li) (iter (+ x1 x2) x1 (list-tail li (- x1 x2)))) (cons (car li)`()))
    
  )
  (iter 1 0 li)
)


;Пример:(Lab11_3 `(123 220 283 23))
(define (Lab11_3 li)
  (define (divisor-number-list x)
    (define (iter n)
      (if (= n 0) `() (if (div x n) (cons n (cons (/ x n) (iter (-- n)))) (iter (-- n))))  
    )
    (remove x (iter (max-square x)))
  )
  (define (iter li) 
    (if (=(length li) 1) #f (or (ormap (λ (el) (friend-test el (car li))) (cdr li)) (iter (cdr li))))
  )
  (define (friend-test x y) 
    (if (and (= (sum-list (divisor-number-list x)) y) (= (sum-list (divisor-number-list y)) x)) (cons x y) #f ) 
  )
  (iter li)
)
;Пример: (Lab11_4 `(1 2 4 8 3 1 2 4 8))
(define (Lab11_4 li)
  (if (< (length li) 3) `() (if (= (/ (cadr li) (car li)) (/ (caddr li) (cadr li))) 
                                 (cons (take li 3) (Lab11_4 (cdr li))) 
                                 (Lab11_4 (cdr li)))) 
)

;Пример: (Lab11_5 `((1 2 321)(3 2 2)( 1 0 9)))
(define (Lab11_5 A)
  (define (iter minN x winner)
    (if (eq? x `()) winner (if (and (< (composition-list (car x)) minN) (andmap isfigure (car x))) 
                               (iter (composition-list (car x)) (cdr x) (car x)) 
                               (iter minN (cdr x) winner)))
  )
  
  (iter +inf.0 A null)
)













