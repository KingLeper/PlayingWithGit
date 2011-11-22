;Alex Manus
;Assignment 10 Redux^4

(load "ParentheC.ss")
(load "pc2c.ss")

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

(define-union kt
  (empty)
  (app-outer rand env k)
  (app-inner v k)
  (punch-out rand env)
  (if-k test conseq alt env k)
  (mult-outer rand2 env k)
  (mult-inner x1 k)
  (zero-k k)
  (sub1-k k))

(define eval-expr
  (lambda (expr env k)
    (union-case expr exp
      [(const n) (apply-k k n)]
      [(var v) (apply-env env v k)]
      [(if test conseq alt)
       (eval-expr test env (kt_if-k test conseq alt env k))]
      [(mult rand1 rand2) (eval-expr rand1 env (kt_mult-outer rand2 env k))]
      [(sub1 rand) (eval-expr rand env (kt_sub1-k k))]
      [(zero rand) (eval-expr rand env (kt_zero-k k))]
      [(letcc body)
           (eval-expr body (envr_extend k env) k)]
      [(throw rator rand) (eval-expr rator env (kt_punch-out rand env))]
      [(lambda body) (apply-k k (clos_closure body env))]
      [(app rator rand) (eval-expr rator env (kt_app-outer rand env k))])))
                            

(define apply-k
  (lambda (k a)
    (union-case k kt
      [(empty) a]
      [(app-outer rand env k) (eval-expr rand env (kt_app-inner a k))]
      [(app-inner v k) (apply-proc v a k)]
      [(punch-out rand env) (eval-expr rand env a)]
      [(if-k test conseq alt env k) (if a
                                        (eval-expr conseq env k)
                                        (eval-expr alt env k))]
      [(mult-outer rand2 env k) (eval-expr rand2 env (kt_mult-inner a k))]
      [(mult-inner x1 k) (apply-k k (* x1 a))]
      [(zero-k k) (apply-k k (zero? a))]
      [(sub1-k k) (apply-k k (sub1 a))])))


(define-union envr
  (empty)
  (extend arg env))

(define apply-env
  (lambda (env num k)
    (union-case env envr
      [(empty) (error 'pc "unbound variable")]
      [(extend arg env)
       (if (zero? num)
           (apply-k k arg)
           (apply-env env (sub1 num) k))])))

(define-union clos
  (closure code env))

(define apply-proc
  (lambda (c a k)
    (union-case c clos
      [(closure code env)
       (eval-expr code (envr_extend a env) k)])))

(define empty-k
  (lambda ()
    (lambda (v)
      v)))


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
             (envr_empty) (kt_empty)))



; Test of letcc/throw...should evaluate to 12.
(pretty-print
  (eval-expr (exp_letcc
                (exp_mult (exp_const 5)
                          (exp_throw (exp_var 0) (exp_mult (exp_const 2)
                                                           (exp_const 6)))))
             (envr_empty) (kt_empty)))