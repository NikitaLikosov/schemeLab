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



;Начало решения задач лабораторной
; Матрица задаётся строками где элементы разделены пробелами пример
#|
1 0 0 
0 0 1
0 1 0
|#
;Пример:(Lab20_1 "in.txt")
(define (Lab20_1 pathIn)
  (define in (open-input-file pathIn))
  (define (next) (read-char in))
  (define (next-line) (read-line in))
  (define (close) (close-input-port in))
  (define (iter n count)
    (if (equal? n eof) (begin (close) 0)
        (+ (if (andmap zero? (map string->number (remove (list-ref (string-split n) count) (string-split n)))) 1 0) (iter (next-line) (add1 count))))
    )
  (iter (next-line) 0)
  )


(define (read-list-file path)
  (define in (open-input-file path))
  (define (next-line) (read-line in))
  (define (close) (close-input-port in))
  (define (iter n)
    (if (eq? n eof) null
        (cons (map string->number (string-split n)) 
              (iter (next-line))))
    )
  (cons (string->number (car (string-split (next-line)))) (iter (next-line))) 
  )


;Пример:(Lab20_2 "in.txt")
(define (Lab20_2 pathIn)
  (define res (read-list-file pathIn))
  (define k (car res))
  
  (andmap (λ (el i) (equal? (sort (remove i (remove-duplicates (cdr el))) <) 
                            (remove i (build-list k values)))) (cdr res) (build-list k values))
  )


;Пример:(Lab20_3 "in.txt")
(define (Lab20_3 pathIn pathOut)
  (define out (open-output-file pathOut #:exists 'replace))
  (define (disp-line str) (begin (display str out)(newline out)))
  (define (disp str) (display str out))
  (define (close) (close-output-port out))
  
  (define res (read-list-file pathIn))
  (define k (car res))
  
  (define (create-edge x y)
    (if (< x y) (cons x y) (cons y x))
    )
  (define (iter li count)
    (if (empty? li) null
        (append (map (λ (x) (create-edge x count))
                     (cdar li))
                (iter (cdr li) (add1 count))))
    )
  (begin (map disp-line (map (λ (el) (string-append (number->string (car el)) " " (number->string (cdr el)))) (remove-duplicates (iter (cdr res) 0)))) 
         (close))
  )


;Пример:(Lab20_4 "in.txt")
(define (Lab20_4 pathIn)
  (define in (open-input-file pathIn))
  (define (next) (read-char in))
  (define (next-line) (read-line in))
  (define (close) (close-input-port in))
  (define (create-edge x y)
    (if (< x y) (cons x y) (cons y x))
    )
  (define (iter n count)
    (if (equal? n eof) (begin (close) null)
        (append (foldl (λ (el index acc) (if (eq? el 0) acc 
                                             (cons (create-edge count index) acc)))
                       null
                       (map string->number (string-split n)) 
                       (build-list (length (string-split n)) add1)) 
                (iter (next-line) (add1 count))))
    )
  (remove-duplicates (iter (next-line) 1))
  )

;Пример:(Lab20_5 "in.txt" "out.txt")
(define (Lab20_5 pathIn pathOut)
  (define in (open-input-file pathIn))
  (define (next) (read-char in))
  (define (next-line) (read-line in))
  
  (define out (open-output-file pathOut #:exists 'replace))
  (define (disp-line str) (begin (display str out)(newline out)))
  (define (disp str) (display str out))
  (define (close) (begin (close-output-port out)(close-input-port in)))
  
  (define (create-str li) (build-list lg (λ (i) (if (member i li) 1 0))))
  (define (iter n count)
    (if (eq? n eof) (close) 
        (begin (disp-line (string-join (map number->string (create-str (map string->number (string-split n))))))
               (iter (next-line) (add1 count))))
    )
  (define lg (string->number (car (string-split (next-line)))))
  (iter (next-line) 1)
  )













