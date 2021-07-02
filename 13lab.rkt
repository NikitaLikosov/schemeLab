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


;Начало решения задач лабораторной

;Пример:(Lab13_1 (build-list 100 values) 3)
(define (Lab13_1 N n)
  (define (create-list k) 
  (define (iter li leng) 
    (if (eq? leng n) `()
        (if (= (remainder (car li) 3) k) (cons (car li) (iter (cdr li) (++ leng)))
            (iter (cdr li) leng))
  ))
  (iter N 0) 
    )
  (map list (create-list 0) (create-list 1) (create-list 2))
  
)


;Пример:(Lab13_2 `("A" "B" "A" "C:/" "D" "M" "X" "M" "K"))
(define (Lab13_2 li)
  (define (iter iter-li path)
    (if (empty? iter-li) path
        (if (member #\: (string->list (car iter-li))) (iter (cdr iter-li) (list (car iter-li)))
            (if (empty? path) (iter (cdr iter-li) path) 
                (if (or (empty? (cdr path))  (not (eq? (cadr path)(car iter-li)))) 
                    (iter (cdr iter-li) (cons (car iter-li) path))
                    (iter (cdr iter-li) (cdr path))))))
  )
  (let ([x (iter li `())]) 
    (if (empty? x) #f (string-join(reverse x) "/"))   
  )
)

;Пример:(Lab13_3 `(3 (2 (4 () ()) ()) (2 () (4 () ()))))
(define (Lab13_3 tree)
  (define (iter max count li) 
    (if (empty? li) count
        (if (< max (car li)) (iter (car li) 1 (cdr li))
            (if (= max (car li)) (iter max (++ count) (cdr li))
                (iter max count (cdr li)))))
  )
  (iter -inf.0 0 (flatten tree))
)

;Пример:(Lab13_4 `(3 (2 (4 () ()) ()) (2 () (4 () ()))))
(define (Lab13_4 tree)
  (define (iter tree)
    (if (and (empty? (cadr tree)) (empty? (caddr tree))) 1
        (if (empty? (cadr tree)) (iter (caddr tree))
            (if (empty? (caddr tree)) (iter (cadr tree))
                (+ (iter (cadr tree)) (iter (caddr tree))))))
  )
  (iter tree)
)


;Пример:(Lab13_5 `(3 (2 (4 () ()) (4 () ())) (2 (4 () ()) (4 (4 () ()) (4 () ())))))
(define (Lab13_5 tree)
  (define (iter tree)
    (or (empty? tree)
    (and (eq? (empty? (cadr tree)) (empty? (caddr tree)))
         (iter (cadr tree)) 
         (iter (caddr tree))))
  )
(iter tree)
)





















