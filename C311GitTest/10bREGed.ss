;Alex Manus
;Assignment 10 Redux^4
;With invaluable consultation from David Petro

(load "ParentheC.ss")
(load "pc2c.ss")

(define-registers *rator *rand *expr *env *a *v *x *k)
(define-program-counter *pc)

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

(trace-define eval-expr
  (lambda () ;;;expr env k
    (union-case *expr exp
      [(const n) (set! *a n) (apply-k)]
      [(var v) (set! *x v) (apply-env)]
      [(if test conseq alt) (set! *k (kt_if-k test conseq alt *env *k))
       (set! *expr test) (eval-expr)]
      [(mult rand1 rand2) (set! *k (kt_mult-outer rand2 *env *k))
                            (set! *expr rand1) (eval-expr)]
      [(sub1 rand) (set! *k (kt_sub1-k *k)) (set! *expr rand) 
       (eval-expr)]
      [(zero rand) (set! *k (kt_zero-k *k)) (set! *expr rand) 
       (eval-expr)]
      [(letcc body) (set! *env (envr_extend *k *env)) (set! *expr body)
       (eval-expr)]
      [(throw rator rand) (set! *k (kt_punch-out rand *env)) (set! *expr rator)
       (eval-expr)]
      [(lambda body) (set! *a (clos_closure body *env)) (apply-k)]
      [(app rator rand) (set! *k (kt_app-outer rand *env *k)) (set! *expr rator)
       (eval-expr)])))
                            

(trace-define apply-k
  (lambda () ;;;k a
    (union-case *k kt
      [(empty) *a]
      [(app-outer rand env k) (set! *k (kt_app-inner *a k)) (set! *expr rand)
       (set! *env env) (eval-expr)]
      [(app-inner v k) (set! *k k) (set! *v v) (apply-proc)]
      [(punch-out rand env) (set! *k *a) (set! *expr rand) (set! *env env)
       (eval-expr)]
      [(if-k test conseq alt env k) (if *a
                                        (begin (set! *k k) (set! *expr conseq)
                                        (eval-expr))
                                        (begin (set! *k k) (set! *expr alt)
                                        (eval-expr)))]
      [(mult-outer rand2 env k) (set! *k (kt_mult-inner *a k))
       (set! *env env) (set! *expr rand2) (eval-expr)]
      [(mult-inner x1 k) (set! *k k) (set! *a (* x1 *a)) (apply-k)]
      [(zero-k k) (set! *k k) (set! *a (zero? *a)) (apply-k)]
      [(sub1-k k) (set! *k k) (set! *a (sub1 *a)) (apply-k)])))

(define-union envr
  (empty)
  (extend arg env))

(trace-define apply-env
  (lambda () ;;;env x k
    (union-case *env envr
      [(empty) (error 'pc "unbound variable")]
      [(extend arg env)
       (if (zero? *x)
           (begin (set! *a arg) (apply-k))
           (begin (set! *env env) (set! *x (sub1 *x)) (apply-env)))])))

(define-union clos
  (closure code env))

(trace-define apply-proc
  (lambda () ;;;v a k
    (union-case *v clos
      [(closure code env)
       (set! *expr code) (set! *env (envr_extend *a env)) (eval-expr)])))

(define ee eval-expr)

(define main
  (lambda ()
    (set! *n 5)
    (set! *expr (exp_app
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
                                        (exp_sub1 (exp_var 0)))))))))
    (set! *env (envr_empty))
    (set! *k (kt_empty))
    (ee)
    (printf "Fact Test Yields: ~d\n" *a)
    (set! *expr (exp_letcc
                  (exp_mult (exp_const 5)
                    (exp_throw (exp_var 0) (exp_mult (exp_const 2)
                                             (exp_const 6))))))
    (set! *env (envr_empty))
    (set! *k (kt_empty))
    (ee)
    (printf "Let/cc Test Yields: ~d\n" *a)
    (envr_empty)))




(main)


