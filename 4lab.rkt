#lang scheme


(define (mod10 x)(remainder x 10))

(define (div10 x)(/ (- x (mod10 x)) 10))

(define (div x y)(= (remainder x y) 0))

(define (++ x)(+ x 1))


(define (lab4_1 a b)
  (define (iter x n y) 
  (if (= n 0) (cons x y) {iter (div10 x) (- n 1) (+ y (* (mod10 x) (expt 10 (- b n))))}))
  (iter a b 0)
  )

(define (lab4_2 a)
  {if (empty? a) #t (if (even? (car a)) (lab4_2 (cdr a)) #f)}
  )

(define (lab4_3 a)
  (define (iter x l) 
  (if (empty? (cdr l)) (if (<= x (car l)) (car l) #f) {if (and (<= x (car l)) (<= (car (cdr l)) (car l))) (car l) (iter (car l) (cdr l))}))
  (iter -inf.0 a)
  )


(define (lab4_4 a)
  (define (iter x y) 
  (if (empty? x) y {iter (cdr x) (cons (car x) y)}))
  (iter (cdr a) a)
  )

(define (lab4_5 a k)
  (define (modN10 x) 
    (< x (expt 10 k))
  )
  (define (iter x l n)
  (cond  
    ((= n 3) x)
    ((and (empty? l) (= n 0)) #f)
    ((empty? l) x) 
    (else {if (modN10 (car l)) (iter (* x (car l)) (cdr l) (++ n)) (iter x (cdr l) n)})))
  (iter 1 a 0)
  )