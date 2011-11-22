;3
(define eval-expr-need
  (lambda (expr env)
    (pmatch expr
      [,n (or (number? n) (boolean? n)) n]
      [,x (symbol? x) (let ([b (apply-env env id)]) (unbox b))]
      [(* ,x1 ,x2) ()
       (* (eval-expr-need x1 env)
          (eval-expr-need x2 env))]
      [(sub1 ,x) ()
       (sub1 (eval-expr-need x env))]
      [(zero? ,x) ()
       (zero? (eval-expr-need x env))]
      [(begin ,x1 ,x2) ()
       (begin
         (eval-expr-need x1 env)
         (eval-expr-need x2 env))]
      [(set! ,id ,val) () 
       (let ([b (apply-env env id)])
       (let ([answer (eval-expr-need val env)])
         (set-box! b answer)))]
      [(if ,test ,conseq ,alt) ()
       (if (eval-expr-need test env)
           (eval-expr-need conseq env)
           (eval-expr-need alt env))]
      [(let ([,id ,rand]) ,body) ()
       (let ([rand (pmatch rand
                     [,x (symbol? x) (apply-env env x)]
                     [else (let ([rand (eval-expr-need rand env)])
                             (box (lambda () rand)))])])
         (eval-expr-need body (extend-env id rand env)))]
      [(lambda (,id) ,body) ()
       (closure id body env)]
      [(,rator ,rand) ()
       (let ([rator (eval-expr-need rator env)]
             [rand (pmatch rand
                     [,x (symbol? x) (apply-env env x)]
                     [else (box (lambda () (eval-expr-need rand env)))])])
         (apply-proc rator rand))])))

(define closure (make-closure eval-expr-need))
           
(test "eval-expr-need-fact-5"
  (eval-expr-need fact-5 (base-env))
  120)

(test "eval-expr-need-fact-5-let"
  (eval-expr-need fact-5-let (base-env))
  120)

(test "eval-expr-need-test-lambda"
  (eval-expr-need test-lambda (base-env))
  5)

(test "eval-expr-need-test-let"
  (eval-expr-need test-let (base-env))
  5)

(test-divergence "eval-expr-need-test-let-strictness"
  (eval-expr-need test-let-strictness (base-env)))

(test "eval-expr-need-test-cbn"
  (eval-expr-need test-cbn (base-env))
  5)

(test "eval-expr-need-test-count"
  (eval-expr-need test-count (base-env))
  4)
