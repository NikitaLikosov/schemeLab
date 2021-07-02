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

(define (testChildren tree fn falsex) 
  (if (empty? tree) falsex (fn tree))
  ) 
(define (mapi fn x)
  (map fn (build-list (length x) values) x)
  )
(define (check-duplicates li) (eq? (length li) (length (remove-duplicates li))))


;Начало решения задач лабораторной
;Пример:(Lab24_1 `(0 3 2 6 5))
(define (Lab24_1 perm)
  (define (createAllPermutations el lst)
    (if (empty? (cdr lst)) (list null)
        (append-map (λ (x) (map (λ (y)(cons x y)) (createAllPermutations x (remove el lst)))) (remove el lst))
        )
    )
  (define (calcPermutation x)
    (if (empty? (cdr x)) 0
        (+ (NOD (car x)(cadr x))(calcPermutation (cdr x))))
    )
  (define (searchPermutation lili maxNLi)
    (if (empty? lili) maxNLi
        (searchPermutation (cdr lili) (let(( x(calcPermutation (car lili)))) (if (> x (car maxNLi)) (cons x (car lili)) maxNLi))))
    )
  (cdr (searchPermutation (createAllPermutations null perm) (cons -inf.0 null)))
  )


;Пример:(Lab24_2 `((5 . 5) (-5 . 5) (-5 . -5) (5 . -5)))
(define (Lab24_2 liNode)
  (define (createBulian lst) ;Булиан длиной не меньше 2 без самого множества
    (if (empty? (cddar lst)) lst
        (append lst (createBulian (append-map (λ (item) (map (λ(x)(remove x item)) item)) lst)))
        )
    )
  (define (calcLiNode liNode)
    (define oneCenter (/ (apply + (map car liNode))(length liNode)))
    (define twoCenter (/ (apply + (map cdr liNode))(length liNode)))
    (+ (apply + (map (λ (x) (abs (- oneCenter x))) (map car liNode)))
       (apply + (map (λ (x) (abs (- twoCenter x))) (map cdr liNode))))
    )
  (cdr (foldl 
        (λ (x acc)(let ((n (calcLiNode x)))(cond 
                                             ((> n (car acc)) (cons n x))
                                             ((equal? n (car acc))(cons n (cons x (cdr acc))))
                                             (else acc)))) 
        (cons -inf.0 null) (cdr (createBulian (list liNode)))))
  )


;Пример:(Lab24_3 12 `(1 2 4 6 3 5 7 8)) 
(define (Lab24_3 startNumber permutation) ;честный перебор графа в ширну
  (define (iter lst)
    (if (empty? (cdar lst)) (caar lst)
        (let ((newLst (append-map (λ(el)(filter-map (λ(x)
                                                      (if (< 1 (NOD x (caar el))) 
                                                          (cons (cons x (car el))(remove x (cdr el)))
                                                          #f))
                                                    (cdr el)))
                                  lst)))
          (if (empty? newLst) (caar lst) (iter newLst))
          )))
  (reverse (iter (list (cons (list startNumber) permutation))))
  )



;Пример:(Lab24_3 12 `(1 2 4 6 3 5 7 8)) не работает(e_e)
(define (Lab24_3_bruteforce startNumber permutation)
  (define (createBulian lst) ;Булиан
    (define (iter lst)
      (if (empty? (cdar lst)) (cons lst null)
          (cons lst (createBulian (append-map (λ (item) (map (λ(x)(remove x item)) item)) lst)))
          ))
    (map (λ (x) (append-map (λ (x)(if (empty? x) x (createAllPermutations null x))) (remove-duplicates x))) (iter lst))
    )
  (define (createAllPermutations el lst)
    (if (empty? (cdr lst)) (list null)
        (append-map (λ (x) (map (λ (y)(cons x y)) (createAllPermutations x (remove el lst)))) (remove el lst))
        )
    )
  (define (testPermutation perm) 
    (if (foldl (λ (x acc)(if (and acc (< 1 (NOD x acc))) x #f)) startNumber perm) perm #f) 
    )
  
  (ormap (λ (x) (member `(3 6 2 4 8) x)) (createBulian (list permutation)))
  ;(testPermutation  `(3 6 2 4 8))
  ;(ormap (λ(x)(ormap testPermutation x)) (createBulian (list permutation)))
  )









