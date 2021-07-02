#lang scheme
;Начало решения задач лабораторной


;Пример:(Lab19_3 "in.txt")
(define (Lab19_3 pathIn)
  (define in (open-input-file pathIn))
  
  (define (close) (close-input-port in))
  
  
  (define (is-func? expr)  
    (and (list? expr) (equal? (car expr) 'define) (or (list? (cadr expr)) (pair? (cadr expr)))))
  
  (define (count-functions expr) 
    (if (or (not (list? expr))(empty? expr))
        0
        (if (is-func? expr) (foldl (λ (x acc) (+ acc (count-functions x))) 1 (caddr expr))
            (foldl (λ (x acc) (+ acc (count-functions x))) 0 expr))))
  
  (define (iter)
    (with-handlers ([exn:fail? (lambda (ex)
                                 (iter))])
      (define expr (read in))
      (if (eof-object? expr)
          (begin (close) 0)
          (+ (count-functions expr) (iter))))) 
  
  
  (iter)


)


#|
;Пример:(Lab19_4 "in.txt")
(define (Lab19_4 pathIn)
  (define in (open-input-file pathIn))
  (define (next) (read-char in))
  (define (next-line) (read-line in))
  
  
  (define (create-file-list lst pathOut) ( 
                                          (define out (open-output-file pathOut #:exists 'replace))
                                          (begin (display (string-join lst " ") out) (close-output-port out))))
  
  (define (create-out-file lst) (begin (create-file-list lst (car lst) "out1.txt") (create-file-list lst (cadr lst) "out2.txt")))
  
  #|
  state 0 - просто код
  state 1 - однострочный коментарий
  state 2 - был символ #
  state 3 - однострочнй коментарий #! с подставой в виде \
  state 4 - многочтрочный коментарий #| |# 
  state 5 - define 
  state 6 - check "("
  state 7 - комент
|#
  
  (define (iter n state token acc lLi rLi)
    (if (eq? n eof) (begin (close) (cons lLi rLi))
        (cond 
          ((eq? state 0) (cond 
                           ((eq? n #\;)(begin (next-line) (iter (С))))))
          ((eq? state 1) (cond 
                           ((eq? n)())))
          ((eq? state 2) (cond 
                           ((eq? n)())))
          ((eq? state 3) (cond 
                           ((eq? n)())))
          )
        )
    )
  (iter (next) 0 0 null null null null)
  )
|#