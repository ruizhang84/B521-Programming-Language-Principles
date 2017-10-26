#lang racket
(require racket/trace)
(define lex
    (lambda (e ls)
      (match e
        (`,x #:when (number? x) `(const ,x))
        (`(zero? ,x) `(zero? ,(lex x ls)) )
        (`(sub1 ,x) `(sub1 ,(lex x ls)) )
        (`(* ,x , y) `(* ,(lex x ls) ,(lex y ls)))
        (`(if ,test ,consq ,alt) `(if ,(lex test ls)
                                   ,(lex consq ls)
                                   ,(lex alt ls)))
        (`(let ((,x ,e)) ,b) `(let ,(lex e ls)
                                ,(lex b (cons x ls))))
        (`(lambda (,x) ,body) `(lambda ,(lex body (cons x ls))))
        (`(,rator ,rand) `(,(lex rator ls) ,(lex rand ls)))
        (`,x (if (index-of ls x) `(var ,(index-of ls x))
                                    '()))
        )))

;part II
;;tagged lists
(define empty-env
  (lambda()
    '()))
(define extend-env
  (lambda (x arg env)
    (cons (cons x arg) env)))
(define apply-env
  (lambda (x env)
    (cond
      [(empty? env)  (error 'value-of "unbound variable ~s" x)]
      [else (if (eqv? x (car (car env)))
                (cdr (car env))
                (apply-env x (cdr env)))]
      )))

;functional
(define closure-fn
  (lambda (body x env)
    (lambda (a)
      (value-of-fn body (extend-env x a env)))))
(define apply-closure-fn
  (lambda (clos a)
    (clos a)))
(define value-of-fn
    (lambda (exp env)
      (match exp
        [`,x #:when (number? x) x]
        [`,x #:when (boolean? x) x]
        [`,x #:when (symbol? x) (apply-env x env)]
        [`(lambda (,x) ,body)
           (closure-fn body x env)]
        [`(zero? ,x) (zero? (value-of-fn x env))]
        [`(sub1 ,x) (sub1 (value-of-fn x env))]
        [`(,rator ,rand) (apply-closure-fn
                          (value-of-fn rator env)
                          (value-of-fn rand env))]
        [`(* ,x ,y) (* (value-of-fn x env)
                        (value-of-fn y env))]
        [`(if ,test ,conseq ,alt)
         (if (value-of-fn test env)
             (value-of-fn conseq env)
             (value-of-fn alt env))]
        [`(let ([,x  ,e]) ,body)
         (let ([arg (value-of-fn e env)])
           (value-of-fn body (extend-env x arg env)))]
        )))

;;data-structure
(define closure-ds
  (lambda (b x env)
    `(closure-ds ,b ,x ,env)))
(define apply-closure-ds
  (lambda (clos a)
    (match clos
      [`(closure-ds ,b ,x ,env)
       (value-of-ds b (extend-env x a env))])))
(define value-of-ds
    (lambda (exp env)
      (match exp
        [`,x #:when (number? x) x]
        [`,x #:when (boolean? x) x]
        [`,x #:when (symbol? x) (apply-env x env)]
        [`(lambda (,x) ,body)
         (closure-ds body x env)]
        [`(zero? ,x) (zero? (value-of-ds x env))]
        [`(sub1 ,x) (sub1 (value-of-ds x env))]
        [`(,rator ,rand) (apply-closure-ds
                          (value-of-ds rator env)
                          (value-of-ds rand env))]
        [`(* ,x , y) (* (value-of-ds x env)
                        (value-of-ds y env))]
        [`(if ,test ,conseq ,alt)
         (if (value-of-ds test env)
             (value-of-ds conseq env)
             (value-of-ds alt env))]
        [`(let ([,x  ,e]) ,body)
         (let ([arg (value-of-ds e env)])
           (value-of-ds body (extend-env x arg env)))]
        )))
;;Part III
(define apply-call
  (lambda (rator rand env)
    (match rator
      (`(lambda (,x) ,body)
       (value-of-dynamic body (extend-env x rand env))))))
     

(define value-of-dynamic
    (lambda (exp env)
      (match exp
        [`,x #:when (number? x) x]
        [`,x #:when (boolean? x) x]
        [`(null? ,x) (null? (value-of-dynamic x env))]
        [`(cons ,a ,d)
         (cons (value-of-dynamic a env)
             (value-of-dynamic d env))]
        [`(car ,p)
         (car (value-of-dynamic p env))]
        [`(cdr ,p)
         (cdr (value-of-dynamic p env))]
        [`(quote ,v) v]
        [`(lambda (,x) ,body) `(lambda (,x) ,body)]
        [`(zero? ,x) (zero? (value-of-dynamic x env))]
        [`(sub1 ,x) (sub1 (value-of-dynamic x env))]              
        [`(* ,x , y) (* (value-of-dynamic x env)
                        (value-of-dynamic y env))]
        [`(if ,test ,conseq ,alt)
         (if (value-of-dynamic test env)
             (value-of-dynamic conseq env)
             (value-of-dynamic alt env))]
        [`(let ([,x  ,e]) ,body)
         (let ([arg (value-of-dynamic e env)])
           (value-of-dynamic body (extend-env x arg env)))]
        ;;from the environment from which the procedure is called,
        [`(,rator ,rand) (apply-call (value-of-dynamic rator env)
                                     (value-of-dynamic rand env)
                                     env)]
        [`,x (apply-env x env)]
        )))

;;Brainteasers
(define closure-fn-ri
  (lambda (body x env extend-env-ri value-of-ri)
    (lambda (a)
      (value-of-ri body (extend-env-ri x a env)))))
(define apply-closure-fn-ri
  (lambda (clos a extend-env-ri value-of-ri)
    (clos a)))
(define closure-ds-ri
  (lambda (b x env extend-env-ri value-of-ri)
    `(closure-ds-ri ,b ,x ,env)))
(define apply-closure-ds-ri
  (lambda (clos a extend-env-ri value-of-ri)
    (match clos
      [`(closure-ds-ri ,b ,x ,env)
       (value-of-ri b (extend-env-ri x a env))])))
;;ds
(define empty-env-ds
  (lambda()
    '()))
(define extend-env-ds
  (lambda (x arg env)
    (cons (cons x arg) env)))
(define apply-env-ds
  (lambda (x env)
    (cond
      [(empty? env)  (error 'value-of "unbound variable ~s" x)]
      [else (if (eqv? x (car (car env)))
                (cdr (car env))
                (apply-env-ds x (cdr env)))]
      )))
;;fn
(define empty-env-fn
  (lambda ()
    (lambda (x)
      (error 'value-of "unbound variable ~s" x))))
(define extend-env-fn
  (lambda (x arg env)
    (lambda (y)
      (if (eqv? y x)
          arg
          (env y)))))
(define apply-env-fn
  (lambda (x env)
    (env x)))
;;ri
(define value-of-ri
  (lambda (empty-env-ri extend-env-ri apply-env-ri
                        closure-ri apply-closure-ri)
    (define value-of-ri-recur
      (lambda (exp env)
        (match exp
          [`,x #:when (number? x) x]
          [`,x #:when (boolean? x) x]
          [`,x #:when (symbol? x) (apply-env-ri x env)]
          [`(lambda (,x) ,body)
           (closure-ri body x env extend-env-ri value-of-ri-recur)]
          [`(zero? ,x) (zero? (value-of-ri-recur x env))]
          [`(sub1 ,x) (sub1 (value-of-ri-recur x env))]
          [`(,rator ,rand) (apply-closure-ri
                            (value-of-ri-recur rator env)
                            (value-of-ri-recur rand env)
                            extend-env-ri value-of-ri-recur)]
          [`(* ,x , y) (* (value-of-ri-recur x env)
                          (value-of-ri-recur y env))]
          [`(if ,test ,conseq ,alt)
           (if (value-of-ri-recur test env)
               (value-of-ri-recur env)
               (value-of-ri-recur alt env))]
          [`(let ([,x  ,e]) ,body)
           (let ([arg (value-of-ri-recur e env)])
             (value-of-ri-recur body (extend-env-ri x arg env)))]
          )))
    (lambda (exp)
      (value-of-ri-recur exp empty-env-ri))))
;;Just Dessert
(define alpha-all
  (lambda (exp)
    (match exp
      [`,x #:when (symbol? x) x]
      [`(lambda (,x) ,body)
       (let ((g (gensym (symbol->string x))))
         `(lambda (,g) ,(subst g x (alpha-all body))))]
      [`(,rator ,rand)
       `(,(alpha-all rator) ,(alpha-all rand))])))
(define subst
  (lambda (to from exp)
    (match exp
      [`,x #:when (symbol? x) (if (eqv? x from)
                                  to
                                  x)]
      [`(lambda (,x) ,body)
       `(lambda (,x) ,(subst to from body))]
      [`(,rator ,rand) `(,(subst to from rator) ,(subst to from rand))]
      )))