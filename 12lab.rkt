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
(define (calcPol A x)
  (sum-list (map * A (reverse (build-list (length A) (λ (n) (** x n))))))
)

;Начало решения задач лабораторной

;Пример:(Lab12_1 `(1 2 321 2 2 1 0 9))
(define (Lab12_1 a)
  (reverse (map * (cdr (reverse a))  (build-list-segment 1 (- (length a) 1))))
)

;Пример:(Lab12_2 `(1 2 3) `{1 2 3 4})
(define (Lab12_2 a b)
  (define (iter a b n)
    (append (take a n) (map + (drop a n) b))
  )
  (if (< (length a) (length b))
      (iter b a (- (length b) (length a)))
      (iter a b (- (length a) (length b))))
)
;Пример: Lab12_3 `(1 2 3) `{1 1 1})
(define (Lab12_3 a b)
  (define (iter a n)
    (if (eq? a `()) (cons 0 `()) 
        (Lab12_2 (append (foldr (λ (x akk) (cons (* x (car a)) akk)) `() b) (make-list  n 0)) (iter (cdr a) (-- n)))))
  (iter a (- (length a) 1)) 
)
  

;Пример: (Lab12_4 7 56 1)
(define (Lab12_4 k . pol)
  (define (createPol x n)
    (if (> n x) (cons x `()) (cons (remainder x n) (createPol (/ (- x (remainder x n)) n) n)))
  )
 (foldl (λ (x akk) (+ (* akk 10) x)) 0 (reverse (createPol (foldl (λ (x akk)(+ akk (calcPol (reverse (createPol x 10)) k))) 0 pol) k)))
)
  
;Пример: (Lab12_5 `(1 2 1) 1 0.001) 
(define (Lab12_5 pol x e)
  (if (= (calcPol pol x) 0) x
  (let ([diff (/(calcPol pol x) (calcPol (Lab12_1 pol) x))])
       (if (< (abs diff) e) x (Lab12_5 pol (- x diff) e))  
  ))
)
  
  
  
  
  














