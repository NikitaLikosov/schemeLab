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

;Начало решения задач лабораторной

;Пример:(Lab9_1 `((-6 -1 -1)(-1 -5 -1)(-1 -1 0)))
(define (Lab9_1 a)
  (cdr (foldl (λ (x rez) (let ([n (foldl (λ (x rez) (if (zero? x) (add1 rez) rez)) 0 x)])
                      (if (> n (car rez)) (cons n x) rez))) (cons -1 `()) a))
)

;Пример:(Lab9_2 10)
(define (Lab9_2 n)
  (build-list n (λ (k1) (build-list n (λ (k2) (if (or (and (<= n (+ k1 k2)) (< 0 (- k1 k2))) (and (> (- n 1) (+ k1 k2)) (> 0 (- k1 k2)))) 0 1)))))
)

;Пример:(Lab9_3 `((5 6 4 5) (-2 5 3 7) (8 7 -2 6)))
(define (Lab9_3 a)
  (define (str n) (foldl (λ (x rez) ( cons (list-ref x n) rez)) `() a))
  (define (maxN x) (foldl (λ (x n rez) (cond
                                        ((= x (car rez)) (cons (car rez) (cons n (cdr rez))) )
                                        ((> x (car rez)) (list x n))
                                        (else rez))) (cons -inf.0 `()) x (build-list (length x) values)))
  (define (minN x) (foldl (λ (x n rez) (cond
                                        ((= x (car rez)) (cons (car rez) (cons n (cdr rez))) )
                                        ((< x (car rez)) (list x n))
                                        (else rez))) (cons +inf.0 `()) x (build-list (length x) values)))
 
  (let ([x (foldl (λ (x rez) (append rez (let ([arrmin (minN x)] [arrmax (maxN x)]) 
                                           (let ([x (make-list (length (filter (λ (x) (= (maxstr (str x)) (car arrmin))) (cdr arrmin))) (car arrmin))]
                                                 [y (make-list (length (filter (λ (x) (= (minstr (str x)) (car arrmax))) (cdr arrmax))) (car arrmax))]) 
                                             (if (equal? x y) x (append x y))))))
                  `() a)])
    (if (equal? x `()) #f x)
  )
)


;Пример:(Lab9_4 `((4 9 2) (3 5 7) (8 1 6)))
(define (Lab9_4 a)
  (= 1 (length (remove-duplicates (append (foldl (λ (x rez) (map + x rez)) (car a) (cdr a)) 
                    (map (λ (x) (foldl + 0 x)) a) 
                    (list (foldl (λ (x n rez) (+ rez (list-ref x n))) 0 a (build-list (length a) values)) 
                          (foldl (λ (x n rez) (+ rez (list-ref x (- (length a) n 1)))) 0 a (build-list (length a) values)))))))
)

;Пример:(Lab9_5 `((1 2 3) (3 2 1) (1 3 1)))
(define (Lab9_5 a)
  (ormap (λ (x) (ormap (λ (y) (if (equal? x y) x #f)) a)) (trans a))  
)










