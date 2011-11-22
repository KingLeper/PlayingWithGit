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
  (empty dismount)
  (app-outer rand env k)
  (app-inner v k)
  (punch-out rand env)
  (if-k test conseq alt env k)
  (mult-outer rand2 env k)
  (mult-inner x1 k)
  (zero-k k)
  (sub1-k k))

(define eval-expr
  (lambda () ;;;expr env k
    (union-case *expr exp
      [(const n) (set! *a n) (set! *pc apply-k)]
      [(var v) (set! *x v) (set! *pc apply-env)]
      [(if test conseq alt) (set! *k (kt_if-k test conseq alt *env *k))
       (set! *expr test) (set! *pc eval-expr)]
      [(mult rand1 rand2) (set! *k (kt_mult-outer rand2 *env *k))
                            (set! *expr rand1) (set! *pc eval-expr)]
      [(sub1 rand) (set! *k (kt_sub1-k *k)) (set! *expr rand) 
       (set! *pc eval-expr)]
      [(zero rand) (set! *k (kt_zero-k *k)) (set! *expr rand) 
       (set! *pc eval-expr)]
      [(letcc body) (set! *env (envr_extend *k *env)) (set! *expr body)
       (set! *pc eval-expr)]
      [(throw rator rand) (set! *k (kt_punch-out rand *env)) (set! *expr rator)
       (set! *pc eval-expr)]
      [(lambda body) (set! *a (clos_closure body *env)) (set! *pc apply-k)]
      [(app rator rand) (set! *k (kt_app-outer rand *env *k)) (set! *expr rator)
       (set! *pc eval-expr)])))
                            

(define apply-k
  (lambda () ;;;k a
    (union-case *k kt
      [(empty dismount) (dismount-trampoline dismount)]
      [(app-outer rand env k) (set! *k (kt_app-inner *a k)) (set! *expr rand)
       (set! *env env) (set! *pc eval-expr)]
      [(app-inner v k) (set! *k k) (set! *v v) (set! *pc apply-proc)]
      [(punch-out rand env) (set! *k *a) (set! *expr rand) (set! *env env)
       (set! *pc eval-expr)]
      [(if-k test conseq alt env k) (if *a
                                        (begin (set! *k k) (set! *expr conseq)
                                        (set! *pc eval-expr))
                                        (begin (set! *k k) (set! *expr alt)
                                        (set! *pc eval-expr)))]
      [(mult-outer rand2 env k) (set! *k (kt_mult-inner *a k))
       (set! *env env) (set! *expr rand2) (set! *pc eval-expr)]
      [(mult-inner x1 k) (set! *k k) (set! *a (* x1 *a)) (set! *pc apply-k)]
      [(zero-k k) (set! *k k) (set! *a (zero? *a)) (set! *pc apply-k)]
      [(sub1-k k) (set! *k k) (set! *a (sub1 *a)) (set! *pc apply-k)])))

(define-union envr
  (empty)
  (extend arg env))

(define apply-env
  (lambda () ;;;env x k
    (union-case *env envr
      [(empty) (error 'pc "unbound variable")]
      [(extend arg env)
       (if (zero? *x)
           (begin (set! *a arg) (set! *pc apply-k))
           (begin (set! *env env) (set! *x (sub1 *x)) (set! *pc apply-env)))])))

(define-union clos
  (closure code env))

(define ee
  (lambda () ;;;expr env
    (call/cc (lambda (jumpout)
               (set! *k (kt_empty jumpout))
               (set! *pc eval-expr)
               (mount-trampoline kt_empty *k *pc)))))

(define apply-proc
  (lambda () ;;;v a k
    (union-case *v clos
      [(closure code env)
       (set! *expr code) (set! *env (envr_extend *a env)) (set! *pc eval-expr)])))

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
    (set! *pc eval-expr)
    (mount-trampoline kt_empty *k *pc)
    (printf "Fact Test Yields: ~d\n" *a)
    (set! *expr (exp_letcc
                  (exp_mult (exp_const 5)
                    (exp_throw (exp_var 0) (exp_mult (exp_const 2)
                                             (exp_const 6))))))
    (set! *env (envr_empty))
    (set! *pc eval-expr)
    (mount-trampoline kt_empty *k *pc)
    (printf "Let/cc Test Yields: ~d\n" *a)
    (envr_empty)))




(main)




