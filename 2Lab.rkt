#lang scheme


(define (mod10 x)(remainder x 10))

(define (div10 x)(/ (- x (mod10 x)) 10))

(define (div x y)(= (remainder x y) 0))


(define (lab2_1 a)
  (lab2_1_r a 9)
  )

(define (lab2_1_r n x)
  (if (= n 0) x (lab2_1_r (div10 n) (min x (mod10 n))))
  )

(define (lab2_2 a)
  (lab2_2_r (div10 a) (mod10 a))
  )

(define (lab2_2_r n x)
  (if (= n 0) #t (if (= x (mod10 n)) (lab2_2_r (div10 n) x) #f) )
  )


(define (lab2_3 a)
  
  (lab2_3_r a 1 1)
  )

(define (lab2_3_r n x y)
  (if (= n x) #t (if (< x n) (lab2_3_r n (* x y) (+ 1 y)) #f) )
  )



(define (lab2_4 a)
  (lab2_4_r a 1 1 (- a 1))
  )

(define (lab2_4_r n x y z)
  (if (< (abs (- n x)) (abs (- n (+ x y)))) x (lab2_4_r n (+ x y) x (abs (- n x))))
  )


(define (lab2_5 a)
  (lab2_5_r a 0 (/ (- a (remainder a 2)) 2))
  )

(define (lab2_5_r n x z)
  (if (< z 1) (= n x) (lab2_5_r n (if (div n z) (+ z x) x) (- z 1)) )
  )

