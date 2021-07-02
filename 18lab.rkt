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
;Пример:(Lab18_1 `(3 7 4 9 2 4))
(define (Lab18_1 li)
  (if (empty? li) li
      (let ([res (foldl (λ (el acc) (cond 
                                      ((< el (car li)) (list (cons el (car acc)) (cadr acc) (caddr acc)))
                                      ((eq? el (car li)) (list (car acc) (cons el (cadr acc)) (caddr acc)))
                                      ((> el (car li)) (list (car acc) (cadr acc) (cons el (caddr acc)))))) (list `() `() `()) li)])
        (if (and (empty? (car res)) (empty? (caddr res))) (cadr res) 
            (append (Lab18_1 (car res)) (Lab18_1 (cadr res)) (Lab18_1 (caddr res))))))
  )

;Пример:(Lab18_2 `(3 7 4 93 21 4))
(define (Lab18_2 li)
  (define (sum-number n)
    (if (eq? (div10 n) 0) n
        (+ (mod10 n) (sum-number (div10 n)))))
  (define (insert li n)
    (if (and (not (empty? li)) (> (sum-number (car li)) (sum-number n)))
        (cons (car li) (insert (cdr li) n))
        (cons n li))
    )
  (define (iter newLi li)
    (if (empty? li) newLi
        (iter (insert newLi (car li)) (cdr li)))
    )
  (iter `() li)
  )

;Пример:(Lab18_3 "in.txt")
(define (Lab18_3 path)
  (define in (open-input-file path))
  (define (next) (read-char in))
  (define (close-in) (close-input-port in))
  
  (define out (open-output-file "getOut.txt" #:exists 'replace))
  (define (next-line str) (begin (display str out)(newline out)))
  (define (close-out) (close-output-port out))
  
  (define (isNumb? x)(or (eq? x #\0)(eq? x #\1)(eq? x #\2)(eq? x #\3)(eq? x #\4)(eq? x #\5)(eq? x #\6)(eq? x #\7)(eq? x #\8)(eq? x #\9)))
  
  (define (iter n sum amound acc)
    (cond
      ((and (equal? n eof)(empty? acc)) (begin (next-line sum)(next-line amound)(close-out)(close-in)))
      ((equal? n eof) (begin (begin (next-line sum)(next-line amound)(next-line (list->string (reverse acc)))(close-out)(close-in))))
      ((isNumb? n) (iter (next) sum amound (cons n acc)))
      ((not (empty? acc)) (begin (next-line (list->string (reverse acc)))(iter (next) (+ sum (string->number (list->string (reverse acc)))) (++ amound) null)))
      (else (iter (next) sum amound acc)))
    )
  (iter (next) 0 0 null)
  )


;Пример:(Lab18_4 "in.txt")
(define (Lab18_4 path)
  (define in (open-input-file path))
  (define (next) (read-char in))
  
  (define out (open-output-file "getOut.txt" #:exists 'replace))
  (define (next-line str) (begin (display str out)(newline out)))
  (define (displayStr str) (display str out))
  (define (close) (begin (close-output-port out)(close-input-port in)))
  
  
  (define (iter n acc accFlag)
    (cond
      ((equal? n eof) (begin (displayStr (list->string (reverse acc)))(close)))
      ((equal? n #\-) (iter (next) acc #t))
      ((and (not (empty? acc)) (equal? n #\space)) (begin (displayStr (list->string (reverse acc))) (iter (next) null #f)))
      ((and accFlag (or (equal? n #\newline)(equal? n #\return))) (iter (next) null #t))
      ((not (empty? acc)) (iter (next) (cons n acc) #f))
      (else (begin (displayStr n) (iter (next) null #f)))
      )
    )
  (iter (next) null #f)
  )


;Пример:(Lab18_5 "in.txt" "words.txt")
(define (Lab18_5 pathIn pathWords)
  (define in (open-input-file pathIn))
  (define (next) (read-char in))
  
  (define out (open-output-file "getOut.txt" #:exists 'replace))
  (define (next-line str) (begin (display str out)(newline out)))
  (define (displayStr str) (display str out))
  (define (close) (begin (close-output-port out)(close-input-port in)))
  
  (define (createVoc doc)
    (define el (read-line doc))
    (if (eq? el eof) (begin (close-input-port doc) `())
        (cons (string-split el) (createVoc doc))
        )
    )
  (define voc (createVoc (open-input-file pathWords)))
  
  (define (testWord word)
    (if (member word (map car voc))
        (foldl (λ (x acc) (if (equal? (car x) word) (cadr x) acc)) null voc) 
        word))
  (define (iter n acc)
    (cond 
      ((eq? n eof) (begin (displayStr (testWord (list->string (reverse acc))))(close)))
      ((and (or (eq? n #\space)(eq? n #\return)(eq? n #\newline)) (not (empty? acc)))
       (begin (displayStr (list->string (append (string->list (testWord (list->string (reverse acc)))) (list n)))) (iter (next) null)))
      (else (iter (next) (cons n acc)))
    )
  )
  (iter (next) null)
 )
 





























































