;2
(define eval-expr-name
  (lambda (expr env)
    (pmatch expr
      [,n (or (number? n) (boolean? n)) n]
      [,x (symbol? x) (let ([b (apply-env env id)]) (let ([Th (unbox b)]) Th))]
      [(* ,x1 ,x2) ()
       (* (eval-expr-name x1 env)
          (eval-expr-name x2 env))]
      [(sub1 ,x) ()
       (sub1 (eval-expr-name x env))]
      [(zero? ,x) ()
       (zero? (eval-expr-name x env))]
      [(begin ,x1 ,x2) ()
       (begin
         (eval-expr-name x1 env)
         (eval-expr-name x2 env))]
      [(set! ,id ,val) () 
       (let ([b (apply-env env id)])
       (let ([answer (eval-expr-name val env)])
         (set-box! b answer)))]
      [(if ,test ,conseq ,alt) ()
       (if (eval-expr-name test env)
           (eval-expr-name conseq env)
           (eval-expr-name alt env))]

      [(let ([,id ,rand]) ,body) ()
       (let ([rand (pmatch rand
                     [,x (symbol? x) (apply-env env x)]
                     [else (let ([rand (eval-expr-name rand env)])
                             (box (lambda () rand)))])])
         (eval-expr-name body (extend-env id rand env)))]
      [(lambda (,id) ,body) ()
       (closure id body env)]
      [(,rator ,rand) ()
       (let ([rator (eval-expr-name rator env)]
             [rand (pmatch rand
                     [,x (symbol? x) (apply-env env x)]
                     [else (box (lambda () (eval-expr-name rand env)))])])
         (apply-proc rator rand))])))

(define closure (make-closure eval-expr-name))           
           
(test "eval-expr-name-fact-5"
  (eval-expr-name fact-5 (base-env))
  120)

(test "eval-expr-name-fact-5-let"
  (eval-expr-name fact-5-let (base-env))
  120)

(test "eval-expr-name-test-lambda"
  (eval-expr-name test-lambda (base-env))
  5)

(test "eval-expr-name-test-let"
  (eval-expr-name test-let (base-env))
  5)

(test-divergence "eval-expr-name-test-let-strictness"
  (eval-expr-name test-let-strictness (base-env)))

(test "eval-expr-name-test-cbn"
  (eval-expr-name test-cbn (base-env))
  5)

(test "eval-expr-name-test-count"
  (eval-expr-name test-count (base-env))
  8)

