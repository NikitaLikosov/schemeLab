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


;Бинарнве деревья
(define left cadr)
(define right caddr)
(define data car)
(define (isleaf? tree) 
  (and (empty? (left tree)) (empty? (right tree)))
  ) 



;Начало решения задач лабораторной
;Пример:(Lab19_1 "in.txt" "out.txt" "hi" "stop" "RiM")
(define (KP_1 path)
  (define in (open-input-file path))
  (define (next) (read-char in))
  (define (close-in) (close-input-port in))
  
  (define (checkWord x y) (if (eq? x "") y (if (eq? y "") x (if (> (string-length x) (string-length y)) y x)))
    )
  (define (iter n newWord word) 
    (if (eq? n eof) (checkWord word newWord)
        (if (eq? n #\space) (iter (next) "" (checkWord word newWord)) 
            (iter (next) (string-append newWord (make-string 1 n)) word))
        ))
  (iter (next) "" "")
  )

(define (KP_2 li)
  (define (iter li n ))
  (iter li)
  )





(define (KP_3 tree)
  (define (minLi li) 
    (- (foldl max -inf.0 li)(foldl min +inf.0 li)) 
    )
  (define (parseNode tree parrentData)
    (flatten (append (if (empty? (left tree)) null 
                (list (data (left tree)))) 
            (if (empty? (right tree)) null 
                (list (data (right tree)))) 
            (list parrentData)))
    )
  (define (iter tree parrentData) 
    (if (empty? tree) 
        (cons -inf.0 0)
        (let [(x (iter (left tree) (data tree))) (y (iter (right tree)(data tree))) (rez (minLi (parseNode tree parrentData)))]
          (foldl (λ (el acc) (cond 
                               ((eq? el (car acc)) (cons (car acc) (add1 (cdr acc))))
                               ((> el (car acc)) (cons el 1))
                               (else acc))) 
                 (cons -inf.0 0)
                 (cons (car x) (cons (car y) (list rez)))))
        )
    )
  
  (iter tree null)
  )

















