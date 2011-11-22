;Alex Manus
;C311
;10/29/06
;This is based off of Assignment 9 code by David Petro all subsequent
;modifications are my own modeled off of code given to us in lab and lecture.

;Errors: I'm lost in a Set! hell, somehow EVERYTHING gets set to 5 when the
;first var gets evaluated, not only *v and *rand which are explicity assigned
;but somehow the p in the rator rand kontinuation also seems to magically
;become 5 so then the rator is assigned to it and the union case
;in the closure statement fails. I just have no idea what is going on
;here. I tried starting over twice but ended up with the same result
;I'm sure its just an incorrect Set! somewhere, but I truly cannot decipher how
;the rand in the kontinuation is ever getting set to 5 since it is evaluated
;before the variable's value is resolved. Update: I studied the input and can
;how the rand is 5 when factorial is applied onto that constant, but that still
;doesn't help me figure out what to do about it getting sent to apply proc as
;the rator when it should be the rand and fact should be the rator.

(cd "C:/DOCUMENTS AND SETTINGS/ALEX MANUS/MY DOCUMENTS/C311")
(load "ParentheC.ss")

(define-registers *rator *rand *expr *env *v *x *k)
(define-program-counter *pc)
;(define *rator 'rator)
;(define *rand 'rand)
;(define *expr 'expr)
;(define *env 'env)
;(define *pc 'pc)
;(define *v 'a)
;(define *x 'x)
;(define *k 'k)

(define-union exp
  (const n)
  (var v)
  (mult rand1 rand2)
  (sub1 rand)
  (zero? rand)
  (if test conseq alt)
  (call/cc body)
  (lambda body)
  (app rator rand))

(trace-define ee
  (lambda () ;;; expr env k
    (union-case *expr exp
      [(const n)  (begin (set! *k *k) (set! *v n)
       (set! *pc apply-k))]
      [(var v)  (begin (set! *k *k) (set! *x v) (set! *env *env)
       (set! *pc apply-env))]
      [(mult rand1 rand2) (begin (set! *k (kt_*-outer rand2 *env *k)) (set! *env *env)
       (set! *expr rand1) (set! *pc ee))]
      [(sub1 rand) (begin (set! *k (kt_sub1-outer *k)) (set! *env *env)
       (set! *expr rand) (set! *pc ee))]
      [(zero? rand) (begin (set! *k (kt_zero?-outer *k)) (set! *env *env)
       (set! *expr rand) (set! *pc ee))]
      [(if test conseq alt) (begin (set! *k (kt_if-outer conseq alt *env *k))
       (set! *env *env) (set! *expr test) (set! *pc ee))]
      [(call/cc body) (begin (set! *k (kt_call/cc-outer *k))
       (set! *env *env) (set! *expr body) (set! *pc ee))]
      [(lambda body) (begin (set! *k *k) (set! *v (clos_closure body *env))
       (set! *pc apply-k))]
      [(app rator rand) (begin (set! *k (kt_rator/rand-outer rand *env *k)) (set! *env *env)
       (set! *expr rator) (set! *pc ee))])))

(define-label tramp
  (lambda ()
    (*pc)
    (tramp)))

(trace-define eval-expr
  (lambda (expr env)
    (call/cc (lambda (jumpout)
               (set! *k (kt_empty jumpout))
               (set! *env env)
               (set! *expr expr)
               (set! *pc ee)
               (mount-trampoline kt_empty *k *pc)))))

(define-union clos
  (closure code env)
  (closure/cc k))

(trace-define apply-proc
  (lambda () ;;; rator rand k
    (union-case *rator clos
      [(closure code env) (set! *k *k)
       (set! *env (envr_extend *rand env)) (set! *expr code) 
       (set! *pc ee)]
      [(closure/cc k) (set! *k k) (set! *v *rand) (set! *pc apply-k)])))

(define-union envr
  (empty)
  (extend val env))

(trace-define apply-env
  (lambda () ;env num
    (union-case *env envr
      [(empty) (error 'pc "unbound variable")]
      [(apply-k arg)
       (if (zero? *x)
           (begin (set! *v arg) (set! *pc apply-k))
           (begin (set! *x (sub1 *x)) (set! *pc apply-env)))])))

(define-union kt
  (rator/rand-inner p k)
  (rator/rand-outer rand env k)
  (call/cc-outer k)
  (if-outer conseq alt env k)
  (zero?-outer k)
  (sub1-outer k)
  (*-inner v k)
  (*-outer x2 env k)
  (empty dismount))

(trace-define apply-k
  (lambda () ;;; k a
    (union-case *k kt
      [(rator/rand-inner p k) (set! *k k) (set! *rand *v)
       (set! *rator p) (set! *pc apply-proc)]
      [(rator/rand-outer rand env k) (set! *k (kt_rator/rand-inner *v k)) 
       (set! *env env) (set! *expr rand) (set! *pc ee)]
      [(call/cc-outer k) (set! *k k) (set! *rand (closure/cc k))
       (set! *rator *v) (set! *pc apply-proc)]
      [(if-outer conseq alt env k) (set! *k k) (set! *env env)
       (if *v
           (set! *expr conseq)
           (set! *expr alt))
       (set! *pc ee)]
      [(zero?-outer k) (set! *k k) (set! *v (zero? *v)) (set! *pc apply-k)]
      [(sub1-outer k) (set! *k k) (set! *v (sub1 *v)) (set! *pc apply-k)]
      [(*-inner v k) (set! *k k) (set! *v (* v *v)) (set! *pc apply-k)]
      [(*-outer x2 env k) (set! *k (kt_*-inner *v k))
       (set! *env env) (set! *expr x2) (set! *pc ee)]
      [(empty dismount) (dismount-trampoline dismount)])))

(define main
  (lambda ()
    (set! *n 5)
    (set! *pc ee)
    (set! *expr (exp_if (exp_zero? (exp_const 3)) (exp_const 1) (exp_const 2)))
    (set! *env (envr_empty))
    (mount-trampoline kt_empty *k *pc)
    (printf "If Test Yields: ~d\n" *v)
    (set! *n 5)
    (set! *pc ee)
    (set! *expr (exp_sub1 (exp_const 3)))
    (set! *env (envr_empty))
    (mount-trampoline kt_empty *k *pc)
    (printf "Sub1 Test Yields: ~d\n" *v)
    (set! *n 5)
    (set! *pc ee)
    (set! *expr (exp_mult (exp_const 3) (exp_const 2)))
    (set! *env (envr_empty))
    (mount-trampoline kt_empty *k *pc)
    (printf "Mult Test Yields: ~d\n" *v)
    (set! *n 5)
    (set! *pc ee)
    (set! *expr (exp_app (exp_lambda (exp_mult (exp_var 0) (exp_const 2)))
                  (exp_const 4)))
    (set! *env (envr_empty))
    (mount-trampoline kt_empty *k *pc)
    (printf "App Lambda Test Yields: ~d\n" *v)
    (set! *n 5)
    (set! *pc ee)
    (set! *expr (exp_app
                  (exp_app (exp_lambda (exp_var 0)) (exp_lambda (exp_var 0)))
                  (exp_const 5)))
    (set! *env (envr_empty))
    (mount-trampoline kt_empty *k *pc)
    (printf "Deep Lambda Test Yields: ~d\n" *v)
    (set! *n 5)
    (set! *pc ee)
    (set! *expr (exp_app
                  (exp_app (exp_lambda (exp_var 0)) (exp_lambda (exp_var 0)))
                  (exp_const 5)))
    (set! *env (envr_empty))
    (mount-trampoline kt_empty *k *pc)
    (printf "Recursive Mult. Test Yields: ~d\n" *v)))
    ;(((lambda (x) (lambda (y) (* x y)) (* 2 3)) (* 3 2)))
;try nested mult. etc.

(main)


;END