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


;Пример: (Lab8_1 `((1 1 1) (1 1 1) (1 1 1)) `((1 1 1) (1 1 1) (1 1 1)))
(define (Lab8_1 a b)  
  (map (lambda (x y) (map + x y)) a b)
)

;Пример: (Lab8_2 `((1 1 1) (1 1 1) (1 1 1) (1 1 -5)))
(define (Lab8_2 a)
  (andmap (lambda (x) (andmap positive? x)) a)
)

;Пример: (Lab8_3 `((0 1 1)(0 0 1)(0 0 0)))       
(define (Lab8_3 a)
  (number? (foldl (lambda (x n) (if (and n (andmap zero? (take x n))) (add1 n) #f)) 1 a))
)

;Пример:(Lab8_4 `((-6 -1 -1)(-1 -5 -1)(-1 -1 -6)))                
(define (Lab8_4 a)
 (let ([a (map (lambda (x) (map abs x)) a)])
 (let ([sum (foldl (lambda (x n rez) (cons (+ (car rez) (- (foldl + 0 x) (list-ref x n))) (cons (list-ref x n) (cdr rez)))) (cons 0 `()) a (build-list (length a) values))])
    (equal? (cons #t #t) (foldl (lambda (x rez) (if (car rez) 
                                  (cons (>= x (car sum)) (or (cdr rez) (> x (car sum)))) 
                                  rez)) (cons #t #f) (cdr sum)))
 ))
)


;Пример: (Lab8_5 10)             
(define (Lab8_5 n)
  (build-list (- n 1) (lambda (x) (build-list (- n 1) (lambda (y) (if (> (- (+ y 1) x) 0) (- (+ y 1) x) (+ n -1 (- (+ y 1) x)))))))
)

;Пример: (Lab8_6 `((0 0 0 0)(-1 -6 -1)(0 0  0)))             
(define (Lab8_6 a)
  (foldl (lambda (x rez) (if (andmap zero? x) (add1 rez) rez)) 0 a)
)










