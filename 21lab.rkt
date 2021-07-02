#lang scheme

;quotient
(define (mod10 x)(remainder x 10))
(define (div10 x)(/ (- x (mod10 x)) 10))
(define (div x y)(= (remainder x y) 0))

(define (++ x)(+ x 1))
(define (-- x)(- x 1))
(define (** x n) 
  (if (= n 0) 1 (* x (** x (-- n))))
  )
(define (build-list-segment x y) 
  (build-list (+ (- y x) 1) (λ (el) (+ el x)))  
  )

(define (mapi fn x)
  (map fn x (build-list (length x) values))
  )

;Начало решения задач лабораторной
;Пример:(Lab21_1 `((3) (4)(1)(4)()(1 0))))
;Пример2 с номерами вершин первым элементом списка:(mapi (λ (el i) (cons i el))(Lab21_1 `((3) (4)(1)(4)()(1 0))))
(define (Lab21_1 li)
  (define (iter li st)
    (let ((x (searchVertex li)))
      (if (not x) st
          (iter (mapi (λ (el i) (if (eq? i x) (cons #f (cdr el)) el)) li) (cons (cons x (cdr (list-ref li x))) st)))
      )
    )
  (define (searchVertex li)
    (ormap (λ (el i) 
             (if (and (eq? (car el) #t)
                      (andmap (λ (el) (eq? #f (car (list-ref li el)))) (cdr el))) i #f)) 
           li 
           (build-list (length li) values))
    )
  (define newli (mapi (λ (el i) (cons i el)) (iter (map (λ (el) (cons #t el)) li) null)))
  (define translate (map (λ (el)(cons (car el) (cadr el))) newli))
  (map (λ (el) (map (λ (x) (ormap (λ (item) (if (eq? x (cdr item)) (car item) #f)) translate)) (cddr el))) newli)
  )


;Пример:(Lab21_2 `((0 4)(0 2)(2 5)(2 1)(3 2)(1 3)) `(3 1)))
(define (Lab21_2 edges del)
  (define (deleteEdge)
    (foldl (λ (x acc) (filter (λ (el) (if (member x el) #f el)) acc)) edges del)
    )
  (define (createTransform count n k) 
    (if (eq? count k) null 
        (if (member n del) (createTransform count (add1 n) k)
            (cons (cons count n) (createTransform (add1 count) (add1 n) k))))
    )
  
  (let ((x (deleteEdge))(k (createTransform 0 0 (add1 (- (apply max (flatten edges))(length del))))))
    (define (ftran x) (or 
                       (ormap (λ (el) (if (eq? x (cdr el)) (car el) #f)) k)
                       x))
    (map (λ (el) (cons (ftran (car el))(ftran (cadr el)))) x)
    )
  )



;Пример:(Lab21_3 `((3) (4)(1)(4)()(1 0)))
(define (Lab21_3 liVer)
  (define k (length liVer))
  (mapi (λ (el i)(foldl (λ (x acc) (remove x acc))(build-list k values)(cons i el)))liVer)
  )

;Пример:(Lab21_4 `((2 4) (2 4)(0 1)(4)(0 1 3)(6)(5 7 8)(6)(6) ()))
(define (Lab21_4 liVer)
  (define (step li res)
    (cons     
     (remove-duplicates (apply append 
                               (cons li (filter (λ (el) 
                                                  (if (ormap (λ (y) (member y el)) li) el #f)) res))))
     (filter (λ (el) (if (ormap (λ (y) (member y el)) li) #f el)) res)
     )
    )
  (define (iter li res count)
    (if (empty? li) res
        (iter (cdr li) (step (cons count (car li)) res) (add1 count))
        )
    )
  (iter liVer null 0) 
  )

;Пример:(Lab21_5 `((2 4) (2 4)(0 1)(4)(0 1 3)))
(define (Lab21_5 liVer)
  (define k (length liVer))
  (define (checkEm liCheck liOld count) 
  (if (empty? liCheck) count
      (checkEm (remove-duplicates (flatten (map (λ (x) (filter (λ (x)(not (member x liOld))) (car (list-tail liVer x)))) liCheck))) (append liCheck liOld) (add1 count)))
  )
  (define (iter count)
    (if (eq? k count) null
        (cons (cons count (checkEm (list count) null 0)) (iter (add1 count)))
        )
    )
  (cdr (foldl (λ (x acc) (cond 
                      ((> (cdr x) (car acc)) acc)
                      ((eq? (cdr x) (car acc)) (cons (car acc) (cons (car x) (cdr acc))))
                      (else (cons (cdr x) (list (car x)))))) (cons +inf.0 null) (iter 0)))
  )
























