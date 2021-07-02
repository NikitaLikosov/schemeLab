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
;Пример:(Lab17_1 ++ `(3 (2 (4 () ()) (4 () ())) (2 (3 () ()) (4 (3 () ()) ()))))
(define (Lab17_1 fn tree)
  (if (empty? tree) null
      (list (fn (data tree)) (Lab17_1 fn (left tree)) (Lab17_1 fn (right tree))))
  )

;Пример:(Lab17_2 + 0 `(3 (2 (4 () ()) (4 () ())) (2 (3 () ()) (4 (3 () ()) ()))))
(define (Lab17_2 fn init tree)
  (define (iter tree acc)
    (if (empty? tree) acc
        (iter (right tree) (iter (left tree) (fn (data tree) acc)))) 
    )
  (iter tree init)
  )


;Пример:(Lab17_3 `(3 (2 (4 () ()) (4 () ())) (2 (3 () ()) (4 (3 (2 (1 () ()) ()) ()) ()))))
(define (Lab17_3 tree)
  (define (iter tree)
    (if (equal? (cdr tree) `(()())) tree
        (or 
         (and (not (empty? (left tree))) (not (empty? (right tree))) (list (data tree) (iter (left tree)) (iter (right tree))))
         (and (empty? (left tree)) (iter (right tree)))
         (iter (left tree)))) 
    )
  (iter tree)
  )



;Пример:(Lab17_4 `((1 . 3) (2 . 1) (5 . 2)))
(define (Lab17_4 listTree)
  (define (memberCar x)
    (let ((nlist (filter-map (λ (el) (and (eq? (car el ) x) (list (cdr el) null null))) listTree)))
      (if (eq? (length nlist) 2) nlist
          (if (eq? (length nlist) 1) (cons null nlist)
              (list null null))))
  )
  (define (iter tree) 
    (if (empty? tree) null 
    (let ([newTree (memberCar (data tree))])
      (cons (data tree) (map iter (memberCar (data tree))))
    ))
   )
    (define (createFirstTree edge)
      (list (car edge) (list (cdr edge) null null) null)
      )
    (define (findFirstEdge lst listChild newTree)
      (if (empty? lst) newTree 
      (if (member (caar lst) listChild) (findFirstEdge (cdr lst) listChild newTree) (if (empty? newTree) 
                                                                        (findFirstEdge (cdr lst) listChild (createFirstTree (car lst)))
                                                                        (list (car newTree) (cadr newTree) (list (cdar lst) null null)))))
      )
    (iter (findFirstEdge listTree (map cdr listTree) null))
    )
  
  
  
  
  
  
  
  
  
  
  
  