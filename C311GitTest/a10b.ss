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

(define eval-expr
  (lambda (expr env k)
    (union-case expr exp
      [(const n) (k n)]
      [(var v) (apply-env env v k)]
      [(if test conseq alt)
       (eval-expr test env (lambda (x) (if x
                                           (eval-expr conseq env k)
                                           (eval-expr alt env k))))]
      [(mult rand1 rand2) (eval-expr rand1 env 
                            (lambda (x1)
                              (eval-expr rand2 env (lambda (x2)
                              (k (* x1 x2))))))]
      [(sub1 rand) (eval-expr rand env (lambda (x)
                                         (k (- x 1))))]
      [(zero rand) (eval-expr rand env (lambda (x)
                                         (k (zero? x))))]
      [(letcc body)
           (eval-expr body (envr_extend k env) k)]
      [(throw rator rand) (eval-expr rator env (lambda (v)
                                                  (eval-expr rand env v)))]
      [(lambda body) (k (clos_closure body env))]
      [(app rator rand)
       (eval-expr rator env (lambda (v)
                              (eval-expr rand env (lambda (w)
                                                    (apply-proc v w k)))))])))

(define-union envr
  (empty)
  (extend arg env))

(define apply-env
  (lambda (env num k)
    (union-case env envr
      [(empty) (error 'pc "unbound variable")]
      [(extend arg env)
       (if (zero? num)
           (k arg)
           (apply-env env (sub1 num) k))])))

(define-union clos
  (closure code env))

(define apply-proc
  (lambda (c a k)
    (union-case c clos
      [(closure code env)
       (eval-expr code (envr_extend a env) k)])))

(define apply-k
  (lambda (k a)
    (k a)))

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
             (envr_empty) (empty-k)))



; Test of letcc/throw...should evaluate to 12.
(pretty-print
  (eval-expr (exp_letcc
                (exp_mult (exp_const 5)
                          (exp_throw (exp_var 0) (exp_mult (exp_const 2)
                                                           (exp_const 6)))))
             (envr_empty) (empty-k)))