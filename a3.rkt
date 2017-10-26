#lang racket
(require racket/trace)
;;value-of
#;(define value-of
    (lambda (exp env)
      (match exp
        ;;numbers
        [`,x #:when (number? x) x]
        ;;booleans
        [`,x #:when (boolean? x) x]
        ;;variables;
        [`,x #:when (symbol? x) (env x)]
        ;;lambda-abstraction
        [`(lambda (,x) ,body)
         (lambda (arg)
           (value-of body
                     (lambda (y)
                       (if  (eqv? x y)
                            arg
                            (env y)))))]
        ;;zero?
        [`(zero? ,x) (zero? (value-of x env))]
        ;;sub1
        [`(sub1 ,x) (sub1 (value-of x env))]
        ;;application
        [`(,rator ,rand) ((value-of rator env)
                          (value-of rand env))]
        ;;*
        [`(* ,x ,y) (* (value-of x env)
                      (value-of y env))]
        ;;if
        [`(if ,test ,conseq ,alt)
         (if (value-of test env)
             (value-of conseq env)
             (value-of alt env))]
        ;;let
        [`(let ([,x ,e]) ,body)
         (let ([arg (value-of e env)])
           (value-of body
                     (lambda (y)
                       (if (eqv? y x)
                           arg
                           (env y)))))]
        ;;begin2
        [`(begin2 ,closa ,closb)
         (cdr (cons (value-of closa env) (value-of closb env)))]
        ;;set!
        [`(set! ,x ,n)
          (set x ) ]
          
        )))

;;functional helpers
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

(define value-of-fn
    (lambda (exp env)
      (match exp
        [`,x #:when (number? x) x]
        [`,x #:when (boolean? x) x]
        [`,x #:when (symbol? x) (apply-env-fn x env)]
        [`(lambda (,x) ,body)
         (lambda (arg)
           (value-of-fn body
                     (extend-env-fn x arg env)))]
        [`(zero? ,x) (zero? (value-of-fn x env))]
        [`(sub1 ,x) (sub1 (value-of-fn x env))]
        [`(,rator ,rand) ((value-of-fn rator env)
                          (value-of-fn rand env))]
        [`(* ,x , y) (* (value-of-fn x env)
                        (value-of-fn y env))]
        [`(if ,test ,conseq ,alt)
         (if (value-of-fn test env)
             (value-of-fn conseq env)
             (value-of-fn alt env))]
        [`(let ([,x  ,e]) ,body)
         (let ([arg (value-of-fn e env)])
           (value-of-fn body (extend-env-fn x arg env)))]
        )))

;tagged lists

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

(define value-of-ds
    (lambda (exp env)
      (match exp
        [`,x #:when (number? x) x]
        [`,x #:when (boolean? x) x]
        [`,x #:when (symbol? x) (apply-env-ds x env)]
        [`(lambda (,x) ,body)
         (lambda (arg)
           (value-of-ds body
                     (extend-env-ds x arg env)))]
        [`(zero? ,x) (zero? (value-of-ds x env))]
        [`(sub1 ,x) (sub1 (value-of-ds x env))]
        [`(,rator ,rand) ((value-of-ds rator env)
                          (value-of-ds rand env))]
        [`(* ,x , y) (* (value-of-ds x env)
                        (value-of-ds y env))]
        [`(if ,test ,conseq ,alt)
         (if (value-of-ds test env)
             (value-of-ds conseq env)
             (value-of-ds alt env))]
        [`(let ([,x  ,e]) ,body)
         (let ([arg (value-of-ds e env)])
           (value-of-ds body (extend-env-ds x arg env)))]
        )))

;;fo-eulav
(define fo-eulav
    (lambda (exp env)
      (match exp
        [`,x #:when (number? x) x]
        [`,x #:when (boolean? x) x]
        [`,x #:when (symbol? x) (apply-env-ds x env)]
        [`(,body (,x) adbmal)
         (lambda (arg)
           (fo-eulav body
                     (extend-env-ds x arg env)))]
        [`(,x ?orez) (zero? (fo-eulav x env))]
        [`(,x 1bus) (sub1 (fo-eulav x env))]
        [`(,rand ,rator) ((fo-eulav rator env)
                          (fo-eulav rand env))]
        [`(,x , y *) (* (fo-eulav x env)
                        (fo-eulav y env))]
        [`(,alt ,conseq  ,test fi)
         (if (fo-eulav test env)
             (fo-eulav conseq env)
             (fo-eulav alt env))]
        )))

(define empty-env (λ () (λ (y) (error 'fo-eulav "unbound variable ~s" y))))

;;Brainteasers
(define value-of
    (lambda (exp env)
      (define .globals '())
      (define set-global
        (lambda (x n)
          (set! .globals (cons (cons x n) .globals))))
      (define value-of-rec
        (lambda (exp env)
          (match exp
            ;;numbers
            [`,x #:when (number? x) x]
            ;;booleans
            [`,x #:when (boolean? x) x]
            ;;variables;
            [`,x #:when (symbol? x) (env x)]
            ;;lambda-abstraction
            [`(lambda (,x) ,body)
             (lambda (arg)
               (value-of-rec body
                             (lambda (y)
                               (if (assoc y .globals)
                                   (cdr (assoc y .globals))
                                   (if (eqv? y x)
                                       arg
                                       (env y))))))]
            ;;zero?
            [`(zero? ,x) (zero? (value-of-rec x env))]
            ;;sub1
            [`(sub1 ,x) (sub1 (value-of-rec x env))]
            ;;application
            [`(,rator ,rand) ((value-of-rec rator env)
                              (value-of-rec rand env))]
            ;;*
            [`(* ,x ,y) (* (value-of-rec x env)
                           (value-of-rec y env))]
            ;;if
            [`(if ,test ,conseq ,alt)
             (if (value-of-rec test env)
                 (value-of-rec conseq env)
                 (value-of-rec alt env))]
            ;;let
            [`(let ([,x ,e]) ,body)
             (let ([arg (value-of-rec e env)])
               (value-of-rec body
                             (lambda (y)
                               (if (assoc y .globals)
                                   (cdr (assoc y .globals))
                                   (if (eqv? y x)
                                       arg
                                       (env y))))))]
            ;;begin2    
            [`(begin2 ,closa ,closb)
             (cdr (cons (value-of-rec closa env) (value-of-rec closb env)))]
            [`(set! ,x ,n)
             (set-global x (value-of-rec n env))]
            )))
      (value-of-rec exp env )))

(define value-of-lex
  (lambda (exp env)
    (match exp
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env))]
      [`(zero ,x) (zero? (value-of-lex x env))]
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))
 
(define empty-env-lex 
  (lambda () '()))

(define apply-env-lex
  list-ref)

(define extend-env-lex
  cons)

;;Just Dessert
(define c0 (lambda (f) (lambda (x) x)))
(define c5 (lambda (f) (lambda (x) (f (f (f (f (f x))))))))

(define csub1-env
    (lambda (num base math it-env)
      (if (= num base)
          (if (empty? it-env)
              base
              (car it-env))
          (csub1-env num (math base) math (cons base it-env)))))

(define csub1
  (lambda (num)
    (lambda (math)
      (lambda (base)
        (csub1-env ((num math) base) base math '())
        ))))


