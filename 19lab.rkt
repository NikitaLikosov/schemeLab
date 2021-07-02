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
;Пример:(Lab19_1 "in.txt" "out.txt" "hi" "stop" "RiM")
(define (Lab19_1 pathIn pathOut . parameters)
  (define in (open-input-file pathIn))
  (define (next) (read-char in))
  
  (define out (open-output-file pathOut #:exists 'replace))
  (define (next-line str) (begin (display str out)(newline out)))
  (define (disp str) (display str out))
  (define (close) (begin (close-output-port out)(close-input-port in)))
  
  (define (numb->param n) 
    (list-ref parameters (-- n))
    )
  
  (define (create-numb lst)
    (string->number (list->string (reverse lst)))
    )
  
  (define (iter n state acc)
    (cond 
      ((eq? n eof) (close))
      ((eq? state 0)(cond
                      ((eq? n #\%) (iter (next) 1 null))
                      (else (begin (disp n) (iter (next) 0 null)))))
      ((eq? state 1)(cond
                      ((eq? n #\%) (begin (disp #\%) (iter (next) 0 null)))
                      (else (iter (next) 2 (list n)))))
      ((eq? state 2)(cond 
                      ((eq? n #\%) (begin (disp (numb->param (create-numb acc))) (iter (next) 0 null)))
                      (else (iter (next) 2 (cons n acc)))))
      )
    )
  
  (iter (next) 0 null)
  )




;Пример:(Lab19_2 "in.txt" "out.txt")
(define (Lab19_2 pathIn pathOut)
  (define in (open-input-file pathIn))
  (define (next) (read-char in))
  (define (next-line) (read-line in))
  
  (define out (open-output-file pathOut #:exists 'replace))
  (define (disp-line str) (begin (display str out)(newline out)))
  (define (disp str) (display str out))
  (define (close) (begin (close-output-port out)(close-input-port in)))
  
  (define (test-numb? x) 
    (or (eq? x 0) (and (> (mod10 x)(mod10 (div10 x))) (test-numb? (div10 x))))
    )
  
  (define (create-numb x) 
    (apply + (map string->number (string-split (car (string-split x "\r")) "+" )))
    )
  
  (define (iter n)
    (if (eq? n eof) null
        (let ((x (create-numb n)))
          (if (test-numb? x) (cons x (iter (next-line))) (iter (next-line))) 
          )
        )
    )
  (begin (disp (string-join (map number->string (sort (iter (next-line)) >)) " "))(close))
  )

;Пример:(Lab19_3 "in.txt")
(define (Lab19_3 pathIn)
  (define in (open-input-file pathIn))
  
  (define (close) (close-input-port in))
  
  (define (is-carination? expr)
    (and (or (list? expr) (pair? expr)) (equal? (car expr) 'quote ))) 
  (define (is-func? expr)  
    (and (list? expr) (equal? (car expr) 'define) (or (list? (cadr expr)) (pair? (cadr expr)))))
  
  (define (count-functions expr) 
    (if (or (not (list? expr))(empty? expr))
        0
        (if (is-carination? expr) 0 (if (is-func? expr) (if (list? (caddr expr)) (foldl (λ (x acc) (+ acc (count-functions x))) 1 (caddr expr)) 1)
                                        (foldl (λ (x acc) (+ acc (count-functions x))) 0 expr)))))
  
  (define (iter count)
    (with-handlers ([exn:fail? (λ (ex)
                                 (iter (count)))])
      (define expr (read in))
      (if (eof-object? expr)
          (begin (close) count)
          (iter (+ count (count-functions expr))))))
  (iter 0)
  )

;Пример:(Lab19_4 "in.txt")
(define (Lab19_4 pathIn)
  (define in (open-input-file pathIn))
  
  (define (close) (close-input-port in))
  
  (define (create-file-list lst pathOut) (let ((out (open-output-file pathOut #:exists 'replace)))
                                           (begin (map (λ (x) (begin (display x out) (display " " out))) lst) (close-output-port out))))
  
  (define (create-out-file lst) (begin (create-file-list (car lst) "out1.txt") (create-file-list (cdr lst) "out2.txt")))
  
  (define (is-carination? expr)
    (and (or (list? expr) (pair? expr)) (equal? (car expr) 'quote ))) 
  (define (is-func? expr)
    (and (list? expr) (equal? (car expr) 'define) (or (list? (cadr expr)) (pair? (cadr expr)))))
  
  (define (count-functions expr) 
    (if (or (not (list? expr))(empty? expr))
        null
        (if (is-carination? expr) null 
            (if (is-func? expr) 
                (if (list? (caddr expr)) 
                    (foldl 
                     (λ (x acc) (let ((lifn (count-functions x)))(if (empty? lifn) acc (append acc lifn)))) 
                     (list expr) 
                     (caddr expr)) 
                    (list expr))
                (foldl (λ (x acc) (let ((lifn (count-functions x)))(if (empty? lifn) acc (append acc lifn)))) null expr)))))
  
  (define (iter)
    (with-handlers ([exn:fail? (lambda (ex)
                                 (iter))])
      (define expr (read in))
      (if (eof-object? expr)
          (begin (close) null)
          (let ((x (count-functions expr))) (if (empty? x) (iter) (append x (iter)))))))
  
  (define (is-rec? x)
    (define namef (caadr x))
    (define (iter-rec body)
      (or (equal? body namef) (and (or (list? body) (pair? body)) (ormap iter-rec body)))
      )
    (iter-rec (cddr x))
    )
  (create-out-file (foldl (λ (x acc) (if (is-rec? x) (cons (cons (caadr x) (car acc)) (cdr acc)) (cons (car acc) (cons (caadr x) (cdr acc))))) 
                          (cons null null) (iter)))
  )




















