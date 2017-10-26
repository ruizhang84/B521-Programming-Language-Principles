#lang racket
;;Part 1 Natural Recursion Refresher
(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
         (lambda (n)
           (if (zero? n)
               ls
               (cdr (nth-cdr (sub1 n))))
           )))
      (car (nth-cdr n)))))
(define union
    (lambda (s1 s2)
      (define in-set?
        (lambda (a ls)
          (cond ((empty? ls) #f)
                ((eqv? a (car ls)) #t)
                (else (in-set? a (cdr ls))))))
      (cond ((empty? s1) s2)
            ((in-set? (car s1) s2) (union (cdr s1) s2))
            (else (cons (car s1) (union (cdr s1) s2))))))
(define extend
    (lambda (x pred)
      (lambda (y)
        (or (eqv? x y)
            (pred y)))))
(define walk-symbol
    (lambda (x s)
      (if (assv x s)
          (walk-symbol (cdr (assv x s)) s)
          x)))
;;Part 2: Free, Bound, Lexical Address
(define lambda->lumbda
    (lambda (e)
      (match e
        (`,x #:when (symbol? x) x)
        (`(lambda (,x) ,body) `(lumbda (,x) ,(lambda->lumbda body)))
        (`(,rator ,rand) `(,(lambda->lumbda rator) ,(lambda->lumbda rand)))
        )))
(define var-occurs?
    (lambda (z e)
      (match e
        (`,x #:when (symbol? x) (eqv? z x))
        (`(lambda (,x) ,body) (var-occurs? z body))
        (`(,rator ,rand) (or (var-occurs? z rator)
                             (var-occurs? z rand)))
        )))
(define vars
    (lambda (e)
      (match e
        (`,x #:when (symbol? x) (list x))
        (`(lambda (,x) ,body) (vars body))
        (`(,rator ,rand) (append (vars rator) (vars rand)))
        )))
(define unique-vars
    (lambda (e)
      (match e
        (`,x #:when (symbol? x) (list x))
        (`(lambda (,x) ,body) (unique-vars body))
        (`(,rator ,rand) (union (unique-vars rator) (unique-vars rand)))
        )))
(define var-occurs-free?
    (lambda (z e)
      (match e
        (`,x #:when (symbol? x) (eqv? z x))
        (`(lambda (,x) ,body) (and (not (eqv? z x))
                                   (var-occurs-free? z body)))
        (`(,rator ,rand) (or (var-occurs-free? z rator)
                             (var-occurs-free? z rand)))
        )))
(define var-occurs-bound?
    (lambda (z e)
      (match e
        (`,x #:when (symbol? x) #f)
        (`(lambda (,x) ,body) (or (var-occurs-bound? z body)
                                  (and (eqv? z x)
                                       (var-occurs? z body))))
        (`(,rator ,rand) (or (var-occurs-bound? z rator)
                             (var-occurs-bound? z rand)))
      )))
(define unique-free-vars
    (lambda (e)
      (match e
        (`,x #:when (symbol? x) (list x))
        (`(lambda (,x) ,body) (remv x (unique-free-vars body)))
        (`(,rator ,rand) (union (unique-free-vars rator) (unique-free-vars rand)))
        )))
(define unique-bound-vars
    (lambda (e)
      (match e
        (`,x #:when (symbol? x) '())
        (`(lambda (,x) ,body) (if (var-occurs? x body)
                                  (union (list x)
                                         (unique-bound-vars body))
                                  (unique-bound-vars body)))
        (`(,rator ,rand) (union (unique-bound-vars rator)
                                 (unique-bound-vars rand)))
        )))
(define lex
    (lambda (e ls)
      (match e
        (`,x #:when (symbol? x) (if (index-of ls x)
                                    `(var ,(index-of ls x))
                                    '()))
        (`(lambda (,x) ,body) `(lambda ,(lex body (cons x ls))))
        (`(,rator ,rand) `(,(lex rator ls) ,(lex rand ls)))
        )))
;;Brianteasers
(define walk-symbol-update
    (lambda (x s)
      (if (assv x s)
          (if (walk-symbol-update (unbox (cdr (assv x s))) s)
              (if (set-box!
                   (cdr (assv x s))
                   (walk-symbol-update (unbox (cdr (assv x s))) s))
                  (walk-symbol-update (unbox (cdr (assv x s))) s)
                  'placeholder)
              'placeholder)
          x)))
;;Dessert
(define var-occurs-both?
    (lambda (z e)
      (match e
        (`,x #:when (symbol? x) (values (eqv? z x) #f))
        ;;not free? not bound? -> not occurs!
        (`(lambda (,x) ,body) (let-values (((free? bound?)
                                            (var-occurs-both? z body)))
                                (values (and free? (not (eqv? z x)))
                                        (or bound? (and free? (eqv? z x))))))
                                ; version 1
                                ;(if (and (not free?)
                                ;         (not bound?))
                                ;    #f
                                ;    (values (and free? (not (eqv? z x)))
                                ;            (or bound? (eqv? z x)))
                                ;    )
                                ;))
        (`(,rator ,rand) (let-values (((free1? bound1?)
                                       (var-occurs-both? z rator))
                                      ((free2? bound2?)
                                       (var-occurs-both? z rand)))
                           (values (or free1? free2?) (or bound1? bound2?))))
        )))