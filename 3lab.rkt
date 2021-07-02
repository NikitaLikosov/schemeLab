#lang scheme


(define (mod10 x)(remainder x 10))

(define (div10 x)(/ (- x (mod10 x)) 10))

(define (div x y)(= (remainder x y) 0)) ;quotient

(define (++ x)(+ x 1))


(define (lab3_1 x) ; если любое число делитсья на 0
  (define (iter a) 
  (if (= a 0) #t {if (or (= (mod10 a) 0) (div x (mod10 a))) {iter (div10 a)} #f}))
  (iter x)
  )


(define (lab3_1_v2 x) ; если любое число не делитсья на 0
  (define (iter a) 
  (if (= a 0) #t {if (and (not (= (mod10 a) 0)) (div x (mod10 a))) {iter (div10 a)} #f}))
  (iter x)
  )


(define (lab3_2 a)
  (define (iter x n) 
    (if (> x a) #f {if (= x a) n {iter (* x 2) (++ n)}}) 
  )
  (if (= a 1) 0 (iter 2 1))
  )

(define (lab3_3 a n)
  
  (define (newNumber x) 
    (if (div x 2) {/ x 2} {+ 1 (* x 3)}) 
  )
  (if (= n 1) (list a) (cons a (lab3_3 (newNumber a) (- n 1))))
  )


(define (lab3_4 a)
  (define (sim x n) 
    (if (< x (* n n)) #t (if (div x n) #f (sim x (+ n 1)))) 
  )
  (define (iter x SNumb) 
    (if (> (* x x) a) SNumb {if (and SNumb (div a x) (sim (/ a x) 2)) #f (iter (+ x 1) (or SNumb (and (div a x) (sim (/ a x) 2))))}) 
  )
  (iter 2 #f)
)
;(remainder (- a sum) n)
(define (lab3_5 a)
  (define (createZ x n)
    (if (= n 1) (mod10 x) (createZ (div10 x) (- n 1)))
    ;(list x n)
  )
  (define (create9 x n)
    (if(= n 0) x (create9 (+ (* x 10) 9) (- n 1)))
  )
  (define (createN sum2 n sum)
    (createZ (+ (+ (quotient (- a sum) n) (create9 0 (- n 1))) 1) (- n (remainder (- a sum) n)))
  )
  (define (iter x n sum)
    (if (< a (+ sum (* x n))) {createN (+ sum (* x n)) n sum} {iter (* x 10) (+ 1 n) (+ sum (* x n))})
  )

  (iter 9 1 0)
)