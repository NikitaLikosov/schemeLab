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

(define (testChildren tree fn falsex) 
  (if (empty? tree) falsex (fn tree))
  ) 


;Начало решения задач лабораторной
(define (newListTree listTree n) 
  (foldl (λ (x acc) (if (eq? n (car x)) (cons (cdr x) acc) acc)) `() listTree)
  )
(define (one listTree)
  (newListTree listTree 1)
  )
(define (zero listTree)
  (newListTree listTree 0)
  )


;Пример:(Lab16_3 `((A.(0 0 0)) (Б.(1 0)) (В.(0 1)) (Г.(1 1 0)) (Д.(0 0 1)) (Y.(1 1 1 1)))) 
(define (Lab16_3 tree)
  (define (iter listTree word)
    (if (or (empty? listTree)) #f 
        (if (empty? (cdr listTree))
            (if (not (empty? (car listTree))) (cons (caar listTree) word) #f)
            (or (iter (one listTree) (cons 1 word))
                (iter (zero listTree) (cons 0 word)))))
    )
  (iter (map cadr tree) `())
  )

;Пример:(Lab16_4 `(((("leaf" . B).("leaf" . C)).("leaf" . D)).(("leaf" . A).("leaf" . E))))
(define (Lab16_4 treeH)
  (define (iter treeH)
    (if (eq? (car treeH) "leaf") 1
        (map (λ (x) (* x 2)) (flatten (list (iter (car treeH)) (iter (cdr treeH)))))
        ))
  (iter treeH)
  )

























