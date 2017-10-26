#lang racket
(define countdown
    (lambda (i)
    (cond ((zero? i) '(0))
          (else (cons i (countdown (sub1 i)))))))
(define insertR
    (lambda (x y l)
      (cond ((empty? l) '())
            ((eqv? x (car l)) (cons x (cons y (insertR x y (cdr l)))))
            (else (cons (car l) (insertR x y (cdr l)))))))
(define remv-1st
    (lambda (x l)
      (cond ((empty? l) '())
            ((eqv? x (car l)) (cdr l))
            (else (cons (car l) (remv-1st x (cdr l)))))))
(define list-index-ofv?
    (lambda (x l)
      (cond ((empty? l) 0)
            ((eqv? x (car l)) 0)
            (else (add1 (list-index-ofv? x (cdr l)))))))
(define filter
    (lambda (number? l)
      (cond ((empty? l) '())
            ((number? (car l)) (cons (car l) (filter number? (cdr l))))
            (else (filter number? (cdr l))))))
(define zip
    (lambda (x y)
      (cond ((empty? x) '())
            ((empty? y) '())
            ((empty? (cdr x)) (list (cons (car x) (car y))))
            ((empty? (cdr y)) (list (cons (car x) (car y))))
            (else (cons (cons (car x) (car y)) (zip (cdr x) (cdr y)))))))
(define map
    (lambda (p ls)
      (cond ((empty? ls) '())
            (else (cons (p (car ls)) (map p (cdr ls)))))))
(define append
    (lambda (ls1 ls2)
      (cond ((empty? ls1) ls2)
            ((empty? ls2) ls1)
            (else (cons (car ls1) (append (cdr ls1) ls2))))))
(define reverse
    (lambda (ls)
      (define appendto
        (lambda (ls x)
          (cond ((empty? ls) (list x))
                (else (cons (car ls) (appendto (cdr ls) x))))))
      (cond ((empty? ls) '())
            (else (appendto (reverse (cdr ls)) (car ls))))))
(define fact
    (lambda (n)
      (cond ((zero? n) 1)
            (else (* (fact (sub1 n)) n)))))
(define append-map
    (lambda (p ls)
      (define append
        (lambda (ls1 ls2)
          (cond ((empty? ls1) ls2)
                ((empty? ls2) ls1)
                (else (cons (car ls1) (append (cdr ls1) ls2))))))
      (cond ((empty? ls) '())
            (else (append (p (car ls)) (append-map p (cdr ls)))))))
(define memv
    (lambda (x ls)
      (cond ((empty? ls) #f)
            ((eqv? x (car ls)) ls)
            (else (memv x (cdr ls))))))
(define fib
    (lambda (n)
      (cond ((< n 2) n)
            (else (+ (fib (- n 1))
                     (fib (- n 2)))))))
;;((w . (x . ())) . (y . ( (z . ()) .()))) <=> ((w x) y (z))
(define binary->natural
    (lambda (ls)
      (cond ((empty? ls) 0)
            (else (+ (* 2 (binary->natural (cdr ls))) (car ls) )))))
(define minus
    (lambda (x y)
      (cond ((zero? y) x)
            (else (minus (sub1 x) (sub1 y))))))
(define div
    (lambda (x y)
      (cond ((zero? x) 0)
            ((= y 1) x)
            (else (add1 (div (- x y) y))))))
(define set-difference
    (lambda (s1 s2)
      (define in-list
        (lambda (x ls)
          (cond ((empty? ls) #f)
                ((eqv? x (car ls)) #t)
                (else (in-list x (cdr ls))))))
      (cond ((empty? s1) '())
            ((empty? s2) s1)
            ((in-list (car s1) s2) (set-difference (cdr s1) s2))
            (else (cons (car s1) (set-difference (cdr s1) s2))))))
;; Brainteasers
#;(define add-one
    (lambda (x ls)
      (cond ((empty? ls) '())
            (else (cons (cons x (car ls)) (add-one x (cdr ls)))))))
#;(define extend
    (lambda (ls1 ls2)
      (cond ((empty? ls1) ls2)
            (else (cons (car ls1) (extend (cdr ls1) ls2))))))
#;(define powerset
    (lambda (ls)
      (cond ((empty? ls) '(()))
            (else (extend (add-one (car ls) (powerset (cdr ls))) (powerset (cdr ls)))))))
(define powerset
    (lambda (ls)
      (define add-one
        (lambda (x ls)
          (cond ((empty? ls) '())
                (else (cons (cons x (car ls))
                            (cons (car ls) (add-one x (cdr ls))))))))
      (cond ((empty? ls) '(()))
            (else (add-one (car ls) (powerset (cdr ls)))))))
(define cartesian-product
    (lambda (ls)
      (define add-in
        (lambda (ele lst)
          (cond ((empty? lst) (list ele))
                ((empty? (cdr lst)) (list(cons ele (car lst))))
                (else (cons (cons ele (car lst))
                            (add-in ele (cdr lst)))))))
      (define extend-cartesian
        (lambda (ls1 ls2)
          (cond ((empty? ls1) '())
                (else (append (add-in (car ls1) ls2)
                              (extend-cartesian (cdr ls1) ls2))))))
      (cond ((empty? ls) '(()))
            (else (extend-cartesian (car ls)
                                    (cartesian-product (cdr ls)))))))
(define insertR-fr
    (lambda (x y ls)
      (foldr (lambda (a l)
               (if (eqv? a x)
                   (cons x (cons y l))
                   (cons a l)))
             '()
             ls)))
(define filter-fr
    (lambda (p ls)
      (foldr (lambda (a l)
               (if (p a)
                   (cons a l)
                   l))
             '()
             ls)))
(define map-fr
    (lambda (p ls)
      (foldr (lambda (a l)
               (cons (p a) l))
             '()
             ls)))
(define append-fr
    (lambda (ls1 ls2)
      (foldr (lambda (a l)
               (cons a l))
             ls2
             ls1)))
(define reverse-fr
    (lambda (ls)
      (foldr (lambda (a s1)
               (foldr (lambda (b s2)
                        (cons b s2))
                      (list a)
                      s1))
             '()
             ls)))
(define binary->natural-fr
    (lambda (ls)
      (foldr (lambda (a rest)
               (+ a (* 2 rest)))
             0
             ls)))
(define append-map-fr
    (lambda (p ls)
      (foldr (lambda (a l)
               (append-fr (p a) l))
             '()
             ls)))
(define set-difference-fr
    (lambda (ls1 ls2)
      (foldr (lambda (a l)
               (if (foldr (lambda (x r)
                            (if r
                                (not (eqv? x a))
                                #f))
                          #t
                          ls2)
                   (cons a l)
                   l))
             '()
             ls1)))
(define powerset-fr
    (lambda (ls)
      (foldr (lambda (x rest)
               (foldr (lambda (a l)
                        (cons (cons x a)
                              l))
                      rest
                      rest))
             '(())
             ls)))
;;cartesian-product-fr
(define appendto-fr
    (lambda (ele lst)
      (foldr (lambda (a l)
               (cons a l))
             (list ele)
             lst)))
(define add-in-fr
    (lambda (add-lst rest-lst)
      (foldr (lambda (rest-ele rest-sofar)
               (append-fr (foldr (lambda (add-ele add-sofar)
                                     (appendto-fr (cons add-ele rest-ele) add-sofar))
                                   '()
                                   add-lst)
                            rest-sofar))
             '()
             rest-lst)))
(define cartesian-product-fr
    (lambda (lts)
      (foldr (lambda (cur rest)
               (add-in-fr cur rest))
             '(())
             lts)))
;;Collatz 
(define collatz
  (letrec
    ((odd-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (odd? x)) (collatz (add1 (* x 3)))) 
            (else (recur x))))))
     (even-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (even? x)) (collatz (/ x 2))) 
            (else (recur x))))))
     (one-case
       (lambda (recur)
         (lambda (x)
           (cond
            ((zero? (sub1 x)) 1)
            (else (recur x))))))
     (base
       (lambda (x)
         (error 'error "Invalid value ~s~n" x))))
    (one-case (odd-case (even-case base))) ;; this should be a single line, without lambda
    ))
(define quine
    (for ((i (in-range 2))) (display (list 'quote '(for ((i (in-range 2))) (display (list 'quote i)))))))