#lang scheme

;quotient
(define (mod10 x)(remainder x 10))

(define (div10 x)(/ (- x (mod10 x)) 10))

(define (** x n) 
  (if (= n 0) 1 (* x (** x (- n 1))))
  )

(define left cadr)
(define right caddr)
(define data car)
(define (isleaf? tree) 
  (and (empty? (left tree)) (empty? (right tree)))
  ) 
(define (mapi fn x)
  (map fn (build-list (length x) values) x)
  )
(define (check-duplicates li) (eq? (length li) (length (remove-duplicates li))))


;Начало решения задач лабораторной
; список смежности задаётся  списком со списками с вершинами смежными к данной(без указания количества вершин и без указания колличества смежных к данной вершине вершин) пример
#|
орграф
`(
 (1 2)
 (3 4)
 (5 6)
 ()
 ()
 ()
 ()
)
просто граф
`((1 2 4)(0 2 4)(0 1)(4)(0 1 3))
|#
;Пример:(Lab22_1 `((1 2)(3 4)(5 6)()()()()))
(define (Lab22_1 liVer)
  (define l (length liVer))
  (define (iter count)
    (if (eq? count -1) `()
        (cons 
         (foldl (λ (el i acc) (if (member count el) (cons i acc) acc)) `() liVer (build-list l values))
         (iter (- count 1)))
        )
    )
  (reverse (iter (- l 1)))
  )


;Пример:(Lab22_2 `((1 2 4)(0 2 4)(0 1)(4)(0 1 3)))
(define (Lab22_2 liVer)
  (define l (length liVer))
  
  (define (peresec a b) (foldl (λ (x acc) (if (member x b) (cons x acc) acc)) null a))
  (define (iter li count) 
    (if (empty? li) `()
        (cons (foldl (λ (x acc) (cons (cons count (cons x (peresec (list-ref liVer x) (car li)))) acc)) null (car li)) 
              (iter (cdr li) count))
        )
    )
  (let ((x(remove-duplicates (map (λ (x) (sort x <))
                                  (append* 
                                   (filter-map 
                                    (λ (x) (if (or (not (check-duplicates x))(empty? (cddr x))) #f (map (λ (y) (list (car x) (cadr x) y)) (cddr x)))) 
                                    (append* (iter liVer 0)))))))) 
    (if (empty? x) #f x))
  )

;Пример:(Lab22_3 `(1 (2 (3 () ()) (8 () ())) (4 () (5 () (6 (7 () ()) ())))))
(define (Lab22_3 tree)
  (define vec (make-vector (length (flatten tree))))
  (define (iter tree count parrents)
    (cond  
      ((and (empty? (left tree))(empty? (right tree)))
       (if (empty? parrents)
           (begin (vector-set! vec count null) (add1 count))
           (begin (vector-set! vec count (list (car parrents))) (add1 count))))
      ((and (not (empty? (left tree)))(empty? (right tree)))
       (begin (vector-set! vec count (if (empty? parrents) (list (add1 count)) (list (car parrents) (add1 count)))) 
              (iter (left tree) (add1 count) (cons count parrents))))
      ((and (empty? (left tree))(not (empty? (right tree))))
       (begin (vector-set! vec count (if (empty? parrents) (list (add1 count)) (list (car parrents) (add1 count)))) 
              (iter (right tree) (add1 count) (cons count parrents))))
      (else 
       (let ((rightCount (iter (left tree) (add1 count) (cons count parrents))))
         (begin (vector-set! vec count (if (empty? parrents) (list (add1 count) rightCount) (list (car parrents) (add1 count) rightCount))) 
                (iter (right tree) rightCount (cons count parrents)))))
      )
    )
  (begin (iter tree 0 null)(vector->list vec))
  )

;Взял со старой лабы готовую
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
;Пример:(Lab22_4 (Lab22_3 `(1 (2 (3 () ()) (8 () ())) (4 () (5 () (6 (7 () ()) ()))))))
(define (Lab22_4 liVer)
  (define (iter ver parent)
    (let ((newVer (if (eq? parent null) (cdr ver) (remove parent (cdr ver))))(dataVer (car ver)))
      (cond 
        ((empty? newVer) 
         (list dataVer null null))
        ((empty? (cdr newVer)) 
         (list dataVer (iter (cons (car newVer)(list-ref liVer (car newVer))) dataVer) null))
        (else 
         (list dataVer 
               (iter (cons (car newVer) (list-ref liVer (car newVer))) dataVer) 
               (iter (cons (cadr newVer) (list-ref liVer (cadr newVer))) dataVer)))))
    )
  (and (andmap (λ (x) (and (>=(length x) 1)(<=(length x) 3))) liVer)
       (eq? (- (length liVer) 1) (/(length (flatten liVer)) 2))
       (eq? 1 (length (Lab21_4 liVer)))
       (iter (ormap (λ (x i) (if (eq? (length x) 1) (cons i x) #f)) liVer (build-list (length liVer) values)) null))
  )









