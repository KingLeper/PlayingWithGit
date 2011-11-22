(define-union exp
  (const n)
  (var v)
  (if test conseq alt)
  (mult rand1 rand2)
  (sub1 rand)
  (zero rand)
  (letcc body)
  (throw rator rand)
  (lambda body)
  (app rator rand))

(define eval-expr
  (lambda (expr env)
    (union-case expr exp
      [(const n) n]
      [(var v) (apply-env env v)]
      [(if test conseq alt)
       (if (eval-expr test env)
           (eval-expr conseq env)
           (eval-expr alt env))]
      [(mult rand1 rand2) (* (eval-expr rand1 env) (eval-expr rand2 env))]
      [(sub1 rand) (- (eval-expr rand env) 1)]
      [(zero rand) (zero? (eval-expr rand env))]
      [(letcc body)
       (call/cc
         (lambda (k)
           (eval-expr body (envr_extend k env))))]
      [(throw rator rand) ((eval-expr rator env) (eval-expr rand env))]
      [(lambda body) (clos_closure body env)]
      [(app rator rand)
       (apply-proc (eval-expr rator env) (eval-expr rand env))])))

(define-union envr
  (empty)
  (extend arg env))

(define apply-env
  (lambda (env num)
    (union-case env envr
      [(empty) (error 'pc "unbound variable")]
      [(extend arg env)
       (if (zero? num)
           arg
           (apply-env env (sub1 num)))])))

(define-union clos
  (closure code env))

(define apply-proc
  (lambda (c a)
    (union-case c clos
      [(closure code env)
       (eval-expr code (envr_extend a env))])))


; Factorial of 5...should be 120.
(pretty-print
  (eval-expr (exp_app
                (exp_lambda
                  (exp_app
                    (exp_app (exp_var 0) (exp_var 0))
                    (exp_const 5)))
                (exp_lambda
                  (exp_lambda
                    (exp_if (exp_zero (exp_var 0))
                            (exp_const 1)
                            (exp_mult (exp_var 0)
                                      (exp_app
                                        (exp_app (exp_var 1) (exp_var 1))
                                        (exp_sub1 (exp_var 0))))))))
             (envr_empty)))

; Test of letcc/throw...should evaluate to 12.
(pretty-print
  (eval-expr (exp_letcc
                (exp_mult (exp_const 5)
                          (exp_throw (exp_var 0) (exp_mult (exp_const 2)
                                                           (exp_const 6)))))
             (envr_empty))) 