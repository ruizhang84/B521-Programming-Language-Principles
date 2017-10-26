#lang racket
(require racket/trace)	
;;call-by-value
#;(define val-of-cbv
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                  (val-of-cbv conseq env)
                                  (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`(lambda (,x) ,body) (make-closure-cbv x body env)]
      [`(set! ,x ,rhs) (let ([vrhs (val-of-cbv rhs env)])
                         (set-box! (env x) vrhs))]
      [`(,rator ,x) #:when (symbol? x) (apply-closure (val-of-cbv rator env)
                                                     (apply-env env x))]
      [`(,rator ,rand) (apply-closure (val-of-cbv rator env)
                                      (val-of-cbv rand env))])))

(define empty-env
  (lambda ()
    (lambda (x)
      (error 'value-of "unbound variable ~s" x))))

(define extend-env
  (lambda (x arg env)
    (lambda (y)
      (if (eqv? y x)
          arg
          (env y)))))

(define apply-env
  (lambda (env x)
    (unbox (env x))))

(define make-closure-cbv
  (lambda (x body env)
    (lambda (a)
      (val-of-cbv body (extend-env x a env)))))

(define apply-closure
  (lambda (clos a)
    (clos (box a))))

;;call-by-reference
(define make-closure-cbr
  (lambda (x body env)
    (lambda (a)
      (val-of-cbr body (extend-env x a env)))))

(define apply-closure-cbr
  (lambda (clos a)
    (clos a)))

(define apply-env-cbr
  (lambda (env x)
    (env x)))

(define val-of-cbr
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                  (val-of-cbr conseq env)
                                  (val-of-cbr alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [`(random ,n) (random (val-of-cbr n env))]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`(lambda (,x) ,body) (make-closure-cbr x body env)]
      [`(set! ,x ,rhs) (let ([vrhs (val-of-cbr rhs env)])
                         (set-box! (env x) vrhs))]
      [`(,rator ,x) #:when (symbol? x) (apply-closure-cbr (val-of-cbr rator env)
                                                     (apply-env-cbr env x))]
      [`(,rator ,rand) (apply-closure (val-of-cbr rator env)
                                      (val-of-cbr rand env))])))

;;call-by-name
(define make-closure-cbname
  (lambda (x body env)
    (lambda (a)
      (val-of-cbname body (extend-env x a env)))))

(define apply-env-cbname
  (lambda (env x)
    ((unbox (env x)))))

(define val-of-cbname
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                  (val-of-cbname conseq env)
                                  (val-of-cbname alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbname e1 env) (val-of-cbname e2 env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`,y #:when (symbol? y) (apply-env-cbname env y)]
      [`(lambda (,x) ,body) (make-closure-cbname x body env)]
      [`(set! ,x ,rhs) (let ([vrhs (val-of-cbname rhs env)])
                         (set-box! (env x) vrhs))]
      [`(,rator ,x) #:when (symbol? x) ((val-of-cbname rator env)(env x))]
      [`(,rator ,rand) ((val-of-cbname rator env) (box (lambda () (val-of-cbname rand env))))])))

;;call-by-need
(define unbox/need
  (lambda (b)
    (let ([val ((unbox b))])
      (set-box! b (lambda () val))
      val)))
(define make-closure-cbneed
  (lambda (x body env)
    (lambda (a)
      (val-of-cbneed body (extend-env x a env)))))

(define apply-env-cbneed
  (lambda (env x)
    (unbox/need (env x))))

(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                  (val-of-cbneed conseq env)
                                  (val-of-cbneed alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`,y #:when (symbol? y) (apply-env-cbneed env y)]
      [`(lambda (,x) ,body) (make-closure-cbneed x body env)]
      [`(set! ,x ,rhs) (let ([vrhs (val-of-cbneed rhs env)])
                         (set-box! (env x) vrhs))]
      [`(,rator ,x) #:when (symbol? x) ((val-of-cbneed rator env)(env x))]
      [`(,rator ,rand) ((val-of-cbneed rator env) (box (lambda () (val-of-cbneed rand env))))])))

;;Brainteaser
(define val-of-cbv
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                  (val-of-cbv conseq env)
                                  (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`(lambda (,x) ,body) (make-closure-cbv x body env)]
      [`(set! ,x ,rhs) (let ([vrhs (val-of-cbv rhs env)])
                         (set-box! (env x) vrhs))]
      [`(add1 ,x) (add1 (val-of-cbv x env))]
      [`(quote ,v) v]
      [`(null? ,x) (null? (val-of-cbv x env))]
      [`(let ([,x  ,e]) ,body)
       (let ([arg (box (val-of-cbv e env))])
           (val-of-cbv body (extend-env x arg env)))]
      [`(cons ,a ,b) (cons (val-of-cbv a env)
                            (val-of-cbv b env))]
      [`(cons^ ,a ,b) (cons (box (lambda () (val-of-cbv a env)))
                           (box (lambda () (val-of-cbv b env))))]
      [`(car^ ,a) ((unbox (car (val-of-cbv a env))))]
      [`(cdr^ ,b) ((unbox (cdr (val-of-cbv b env))))]
      [`(,rator ,x) #:when (symbol? x) (apply-closure (val-of-cbv rator env)
                                                     (apply-env env x))]
      [`(,rator ,rand) (apply-closure (val-of-cbv rator env)
                                      (val-of-cbv rand env))]
      )))
