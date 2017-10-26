#lang racket
;(require racket/trace)
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

;1
(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps (cdr n)
                               (lambda (d)
                                 (k (+ (car n)
                                       (* 2 d)))))]
                                    
      )))

;2
(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (lambda (d)
                              (k (* (car ls)
                                    d))))]
      )))

;3
(define times-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-shortcut (cdr ls) (lambda (d)
                              (k (* (car ls)
                                    d))))]
      )))

;4
(define plus-cps
  (lambda (m k)
    (k (lambda (n)
         (+ m n)))))

;5
(define remv-first-9*
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (remv-first-9* (car ls)
                      (lambda (d)
                        (if (equal? (car ls) d)
                            (remv-first-9* (cdr ls) (lambda(v)
                                                     (k (cons (car ls) v))))
                            (remv-first-9* (car ls) (lambda (v)
                                                     (k (cons v (cdr ls)))))
                            )))]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9* (cdr ls) (lambda (d)
                                      (k (cons (car ls) d))))]
      )))


;6
(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps (car ls) (lambda (d)
                                       (cons-cell-count-cps (cdr ls)
                                                            (lambda (v)
                                                              (k (add1 (+ d v)))))))]
      [else (k 0)]
      )))

;7
(define find-cps
  (lambda (u s k)
    (let ((pr (assv u s)))
      (if pr
          (find-cps (cdr pr) s k)
          (k u)))
    ))


;8
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 (lambda (d)
                                       (k d)))]
      [else (ack-cps m (sub1 n)
                     (lambda (d)
                       (ack-cps (sub1 m) d
                            (lambda (v)
                              (k v)))))]
      )))

;9
(define fib-cps
  (lambda  (n k)
    ((lambda (fib k)
       (fib fib n k))
     (lambda (fib n k)
       (cond
         [(zero? n) (k 0)]
         [(zero? (sub1 n)) (k 1)]
         [else (fib fib (sub1 n) (lambda (d)
                                   (fib fib (sub1 (sub1 n))
                                        (lambda (v)
                                          (k (+ d v))))))]))
     
      k)))


;10
(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h k)
       (h h (lambda (d)
               (d seed '() k))))
     (lambda (h k)
       (k (lambda (seed ans k)
         (p seed (lambda (a)
                   (if a
                       (k ans)
                       (h h (lambda (d)
                              (g seed (lambda (v)
                                        (f seed (lambda (b)
                                                  (d v (cons b ans) k)))))))))))))
    k)))
    
(define null?-cps
    (lambda (ls k)
      (k (null? ls))))
(define car-cps
    (lambda (pr k)
      (k (car pr))))
(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))
;(unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (lambda (v) v))

;11
(define empty-s
  (lambda ()
    '()))
 
(define unify-cps
  (lambda (u v s k)
    (find-cps u s
              (lambda (a1)
                (find-cps v s
                          (lambda (a2)
                            (cond
                              [(eqv? a1 a2) (k s)]
                              [(number? a1) (k (cons (cons a1 a2) s))]
                              [(number? a2) (unify-cps a2 a1 s (lambda (d) (k d)))]
                              [(and (pair? a1) (pair? a2))
                               (unify-cps (car a1) (car a2) s
                                          (lambda (d)
                                            (unify-cps (cdr a1) (cdr a2) d
                                                       (lambda (e)
                                                         (if d
                                                             (k e)
                                                             (k #f))))))]
                                                                
                              [else (k #f)]
                              )))))))
                         
;12
(define M-cps
  (lambda (f k)
    (k (lambda (ls k)
      (cond
        [(null? ls) (k '())]
        [else (f (car ls) (lambda (d)
                            (M-cps f (lambda (e)
                                       (cons d (e (cdr ls) k))))))]
        )))))

;13
(define use-of-M-cps
  ((M-cps (lambda (n k) (k (add1 n))) (empty-k))
   '(1 2 3 4 5) (empty-k)))


;;Brainteaser
;14
(define strange-cps
  (lambda (x k)
    ((lambda (g k) (lambda (x k) (g g k)))
     (lambda (g k) (lambda (x k) (g g k)))
     k)))

;15
(define use-of-strange-cps
  (let ([strange^ (strange-cps 5 (lambda (d)
                                     (d 6 (lambda (e)
                                            (e 7 (empty-k))))))])
    (strange^ 8 (lambda (d)
                  (d 9 (lambda (a)
                       (a 10 (empty-k))))))))
;16
(define why-cps
  (lambda (f k)
    ((lambda (g k)
       (f (lambda (x k) (g g (lambda (d) (d x k)))) k))
     (lambda (g k)
       (f (lambda (x k) (g g (lambda (d) (d x k)))) k))
     k)))

#;(define almost-length-cps
    (lambda (f k)
      (k (lambda (ls k)
           (if (null? ls)
               (k 0)
               (k (f (cdr ls) (lambda (d)
                             (add1 d)))))))
      ))


