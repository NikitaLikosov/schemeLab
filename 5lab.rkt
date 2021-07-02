#lang scheme


(define (mod10 x)(remainder x 10))

(define (div10 x)(/ (- x (mod10 x)) 10))

(define (div x y)(= (remainder x y) 0))

(define (++ x)(+ x 1))
(define (-- x)(- x 1))
(define (sim x)
  (define (simplN n)
    (if (< x (* n n)) #t (if (div x n) #f (simplN (+ n 1)))) 
  )
  (simplN 2)
)  

;Пример: (Lab5_1 8) 
(define (Lab5_1 a)
  (define (Nkol n x k)
    (if (div x n) (Nkol n (/ x n) (++ k)) (cons x k))
  )
  
  (define (iter x n li)
  (if (= 1 x) li {if (and (div x n) (sim n)) {if (and (not (empty? li)) (= (caar li) n)) (iter (/ x n) n (cons (cons (caar li) (++ (cdar li))) (cdr li))) (iter (/ x n) n (cons (cons n 1) li))} (iter x (++ n) li)})  
  )
  (iter a 2 `()) 
)


;Пример: (Lab5_2 '(2 1 4 2 5 5 1 4))
(define (Lab5_2 a)
  (define (iterSum li s n)
    (if (empty? li) (/ s n) {iterSum (cdr li) (+ s (car li)) (++ n)})
  )
  (define (iterN li n z) 
  (if (empty? li) n {cond
                      [(< (abs (- z (car li))) (abs (- z n))) (iterN (cdr li) (car li) z)]
                      [(= (abs (- z (car li))) (abs (- z n))) (iterN (cdr li) (min n (car li)) z)]
                      [else (iterN (cdr li) n z)]}))
  (define (iterIndex li x n liR) 
  (if (empty? li) liR {if (= (car li) x) (iterIndex (cdr li) x (++ n) (cons n liR)) (iterIndex (cdr li) x (++ n) liR)}) 
  )
  (if (empty? a) #f (let ([x (iterN (cdr a) (car a) (iterSum a 0 0))]) 
  (cons x (iterIndex a x 0 `()))
  ))
)


;Пример: (Lab5_3 '(1 2 4 8 3))
(define (Lab5_3 a)
  (define (iter li n k t)
    (if (empty? li) (max n k) {if (equal? (even? (car li)) t) (iter (cdr li) (++ n) k t) (iter (cdr li) 1 (max n k) (not t))})
  )
  (if (empty? a) 0 (iter (cdr a) 1 0 (even? (car a))))
)

; Я не до конца понял условие и написал 2 варианта задачи
; Если список начинается с 1 Пример: (Lab5_4V3 `(0 1 2 3))
(define (Lab5_4V3 a)
 (define (iter li n)
   (if (empty? li) `() (if (div (car li) n) (cons (car li) (iter (cdr li) (++ n))) (iter (cdr li) (++ n))))
 )
 (if (empty? a) `() (iter a 1))
)

; Если любое число делитсья на ноль  Пример: (Lab5_4 `(0 1 2 3))
(define (Lab5_4 a)
 (define (iter li n)
   (if (empty? li) `() (if (div (car li) n) (cons (car li) (iter (cdr li) (++ n))) (iter (cdr li) (++ n))))
 )
 (if (empty? a) `() (cons (car a) (iter (cdr a) 1)))
)

; Если любое число не делитсья на ноль  Пример: (Lab5_4V2 `(0 1 2 3))
(define (Lab5_4V2 a) 
 (define (iter li n)
   (if (empty? li) `() (if (div (car li) n) (cons (car li) (iter (cdr li) (++ n))) (iter (cdr li) (++ n))))
 )
 (if (empty? a) `() (iter (cdr a) 1))
)



;Пример: (Lab5_5 '(1 230 456) '(654 32 1))
(define (Lab5_5 a b)
 (define (rotateN x y)
   (if (= x 0) y {rotateN (div10 x) (+ (* y 10) (mod10 x))})
 )
 (or (equal? b (map (lambda (item) (rotateN item 0)) (reverse a))) (equal? a (map (lambda (item) (rotateN item 0)) (reverse b))))
)

