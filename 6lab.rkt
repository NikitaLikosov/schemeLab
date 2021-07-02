
#lang scheme


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



;Пример: (Lab6_1 `(1 2 3 2 4 3 1)) 
(define (Lab6_1 a)
  (define (iter li1 li2 n)
  (if (empty? li1) n {if (memberCust (car li1) li2) (iter (cdr li1) li2 n) (iter (cdr li1) (cons (car li1) li2) (++ n))})  
   )
  (iter a `() 0) 
)
;Пример: (Lab6_2V3 `(1 2 3 2 4 3 1)) 
(define (Lab6_2V3 a)
  (define (newli x li li2 n) 
    (if (empty? li) (cons n li2) {if (equal? x (car li)) (newli x (cdr li) li2 (++ n)) (newli x (cdr li) (cons (car li) li2) n)}) 
  )
  (define (iter li li2) 
  (if (empty? li) li2 (let ([x (newli (car li) (cdr li) `() 1)]) (iter (cdr x) (cons (cons (car li) (car x)) li2))))  
  )
  (iter a `())
)


;Пример: (Lab6_2 `(1 2 3 2 1 3)) 
(define (Lab6_2 a)
  (define (kolEl x li n) 
    (if (empty? li) n (kolEl x (cdr li) (if (equal? x (car li)) (++ n) n)))
  )
  (define (iter li1 li2)
  (if (empty? li1) li2 {iter (cdr li1) (cons (cons (car li1) (kolEl (car li1) a 0)) li2)})  
  )
  (iter a `()) 
)
;Пример с обратным списком (define (Lab6_2V2 a)
(define (Lab6_2V2 a)
  (reverse (Lab6_2 a)) 
)

;Пример: (Lab6_3 `(1 2 3 4 5 6)) 
(define (Lab6_3 a)
  (define (newLi x li li2 bol)
    (if (empty? li) (cons bol li2) (cond 
                                     [(= (car li) x) (newLi x (cdr li) li2 #t)]
                                     [(< (car li) x) (newLi x (cdr li) li2 bol)]
                                     [(> (car li) x) (newLi x (cdr li) (cons (car li) li2) bol)]))
  )
  (define (iter x y li n)
  (if (empty? li) n {let ([z (newLi x li `() #f)]) (iter (+ x y) x (cdr z) (if (car z) (++ n) n))})  
  )
  (iter 1 1 a 0)
)

;Пример: (Lab6_4 `(1 2 3 4) `(3 4 5)) 
(define (Lab6_4 li1 li2)
  (if (or (empty? li1) (empty? li2)) `() {if (= (NOD (car li1) (car li2)) 1) 
                                             (cons (cons (car li1) (car li2)) (Lab6_4 (cdr li1) (cdr li2))) 
                                             (Lab6_4 (cdr li1) (cdr li2))}) 
)


;Пример: (Lab6_5 `(1 2 3 4 5 6)) 
(define (Lab6_5 a)
  (define (iter li li2)
    (if (empty? li) `() {cons (reverse (cons (car li) li2)) (iter (cdr li) (cons (car li) li2)) })
  )
  (iter a `())
)





