#lang scheme
(define (lastz x)(remainder x 10))



(define (lab1_1 a) 
  (define  x (read))
  (define  y (read))
  (if (>= y 0) (- (sqrt (+ (* (abs x) (abs x)) (* (abs y) (abs y)))) (* a a)) (cond 
                                                        ((and (>= (abs x) a) (>= (abs y) a)) (sqrt (+ (* (- (abs x) a) (- (abs x) a)) (* (- (abs y) a) (- (abs y) a)))))
                                                        ((>= (abs x) a) (- (abs x) a))
                                                        ((>= (abs y) a) (- (abs y) a))))
)

(define (lab1_2 x y)
  (abs (- (+ (* 360 (/ x 12)) (* 30 (/ y 60))) (* 360 (/ y 60))))
)

(define (lab1_3_last x) 
  (cond 
    [(= x 0) "ноль"]
    [(= x 1) "один"]
    [(= x 2) "два"]
    [(= x 3) "три"]
    [(= x 4) "четыре"]
    [(= x 5) "пять"]
    [(= x 6) "шесть"]
    [(= x 7) "семь"]
    [(= x 8) "восемь"]
    [(= x 9) "девять"])
)

(define (lab1_3_last2 x) 
  (cond 
    [(= x 10) "десять"]
    [(= x 11) "одинадцать"]
    [(= x 12) "двенадцать"]
    [(= x 13) "тринадцать"]
    [(= x 14) "четырнадцать"]
    [(= x 15) "пятнадцать"]
    [(= x 16) "шестьнадцать"]
    [(= x 17) "семнадцать"]
    [(= x 18) "восемнадцать"]
    [(= x 19) "девятнадцать"])
)

(define (lab1_3_ten x) 
  (cond 
    [(= x 20) "двадцать"]
    [(= x 30) "тридцать"]
    [(= x 40) "сорок"]
    [(= x 50) "пятьдесят"]
    [(= x 60) "шестьдесят"]
    [(= x 70) "семьдесят"]
    [(= x 80) "восемьдесят"]
    [(= x 90) "девяносто"])
)

(define (lab1_3 n)
  (cond 
    [(= n 100) "сто"]
    [(< n 10) (lab1_3_last n)]
    [(< n 20) (lab1_3_last2 n)]
    [(= (lastz n) 0) (lab1_3_ten (- n (lastz n)))]
    [else (string-append (lab1_3_ten (- n (lastz n))) " " (lab1_3_last (lastz n)))]
  ) 
)


(define (lab1_4 x1 x2 y1 y2)
  (if (= (remainder (+ x1 x2) 2) (remainder (+ y1 y2) 2)) (if (= (abs (- x1 y1)) (abs (- x2 y2))) 1 2) #f)
)

(define (lab1_5 x1 x2 y1 y2)
  (>= (- y2 y1) (abs (- x2 x1)))
)