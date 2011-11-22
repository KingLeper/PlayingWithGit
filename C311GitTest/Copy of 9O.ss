; David Petro
; dkpetro
; a9.ss
; Errors: 

(load "pmatch.scm")

(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (let* ((expected expected-result)
            (produced tested-expression))
       (if (equal? expected produced)
           (printf "~s works!\n" title)
           (error
             'test
             "Failed ~s: ~a\nExpected: ~a\nComputed: ~a"
             title 'tested-expression expected produced))))))

(define simple-test
  '((lambda (x)
      (* 3 x)) 3))

(define fact-5
  '((lambda (f)
      ((f f) 5))
    (lambda (f)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))

(define call/cc-fun
  '(((lambda (x)
       (lambda (y)
         (* x (call/cc
                (lambda (k)
                  (* y
                   (call/cc
                     (lambda (j)
                       (* y (j 3))))))))))
     5)
    4))
; /////////////////////////////////////////////////////////////////////////////
; Step 1: RI and DS procs and envs.  CPS afterwards

(define ee
  (lambda (expr env k)
    (pmatch expr
      (,n (or (number? n) (boolean? n)) (apply-k k n))
      (,x (symbol? x) (apply-env env x k))
      ((* ,x1 ,x2) ()
       (ee x1 env (lambda (v)
                    (ee x2 env (lambda (w)
                                 (apply-k k (* v w)))))))
      ((sub1 ,x) () (ee x env (lambda (v) (apply-k k (sub1 v)))))
      ((zero? ,x) () (ee x env (lambda (v) (apply-k k (zero? v)))))
      ((if ,test ,conseq ,alt) ()
       (ee test env (lambda (v)
                      (if v
                          (ee conseq env k)
                          (ee alt env k)))))
      ((call/cc ,rator) () (ee rator env (lambda (v)
                                           (apply-proc v (closure/cc k) k))))
      ((lambda (,id) ,body) () (apply-k k (closure id body env)))
      ((,rator ,rand) () (ee rator env (lambda (p)
                                         (ee rand env (lambda (a)
                                                        (apply-proc p a k)))))))))

(define eval-expr
  (lambda (expr)
    (ee expr (base-env) (empty-k))))

(define apply-proc
  (lambda (rator rand k)
    (pmatch rator
      [(closure ,id ,body ,env) () (ee body (extend-env id rand env) k)]
      [(closure/cc ,k) () (apply-k k rand)])))

(define apply-k
  (lambda (k a)
    (k a)))

(define apply-env
  (lambda (env x k)
    (pmatch env
      [() () (error 'apply-env "Unbound variable ~s" x)]
      [(extend-env ,id ,val ,env) () (if (eq? x id)
                                         (apply-k k val)
                                         (apply-env env x k))])))

(define extend-env
  (lambda (id val env)
    `(extend-env ,id ,val ,env)))

(define base-env
  (lambda ()
    '()))

(define empty-k
  (lambda ()
    (lambda (v)
      v)))

(define closure
  (lambda (id body env)
    `(closure ,id ,body ,env)))

(define closure/cc
  (lambda (k)
    `(closure/cc ,k)))

(test "fact-5 Step 1"
  (eval-expr fact-5)
  120)

(test "call/cc Step 1"
  (eval-expr call/cc-fun)
  60)
; /////////////////////////////////////////////////////////////////////////////
; Step 2: RI and DS Continuations

(define ee
  (lambda (expr env k)
    (pmatch expr
      (,n (or (number? n) (boolean? n)) (apply-k k n))
      (,x (symbol? x) (apply-env env x k))
      ((* ,x1 ,x2) () (ee x1 env (*-outer x2 env k)))
      ((sub1 ,x) () (ee x env (sub1-outer k)))
      ((zero? ,x) () (ee x env (zero?-outer k)))
      ((if ,test ,conseq ,alt) () (ee test env (if-outer conseq alt env k)))
      ((call/cc ,rator) () (ee rator env (call/cc-outer k)))
      ((lambda (,id) ,body) () (apply-k k (closure id body env)))
      ((,rator ,rand) () (ee rator env (rator/rand-outer rand env k))))))

(define eval-expr
  (lambda (expr)
    (ee expr (base-env) (empty-k))))

(define apply-proc
  (lambda (rator rand k)
    (pmatch rator
      [(closure ,id ,body ,env) () (ee body (extend-env id rand env) k)]
      [(closure/cc ,k) () (apply-k k rand)])))

(define apply-env
  (lambda (env x k)
    (pmatch env
      [() () (error 'apply-env "Unbound variable ~s" x)]
      [(extend-env ,id ,val ,env) () (if (eq? x id)
                                         (apply-k k val)
                                         (apply-env env x k))])))
(define apply-k
  (lambda (k a)
    (pmatch k
      [(rator/rand-inner ,p ,k) () (apply-proc p a k)]
      [(rator/rand-outer ,rand ,env ,k) () (ee rand env (rator/rand-inner a k))]
      [(call/cc-outer ,k) () (apply-proc a (closure/cc k) k)]
      [(if-outer ,conseq ,alt ,env ,k) () (if a
                                              (ee conseq env k)
                                              (ee alt env k))]
      [(zero?-outer ,k) () (apply-k k (zero? a))]
      [(sub1-outer ,k) () (apply-k k (sub1 a))]
      [(*-inner ,v ,k) () (apply-k k (* v a))]
      [(*-outer ,x2 ,env ,k) () (ee x2 env (*-inner a k))]
      [(empty-k) () a])))

(define *-inner
  (lambda (v k)
    `(*-inner ,v ,k)))

(define *-outer
  (lambda (x2 env k)
    `(*-outer ,x2 ,env ,k)))

(define sub1-outer
  (lambda (k)
    `(sub1-outer ,k)))

(define zero?-outer
  (lambda (k)
    `(zero?-outer ,k)))

(define if-outer
  (lambda (conseq alt env k)
    `(if-outer ,conseq ,alt ,env ,k)))

(define call/cc-outer
  (lambda (k)
    `(call/cc-outer ,k)))

(define rator/rand-outer
  (lambda (rand env k)
    `(rator/rand-outer ,rand ,env ,k)))

(define rator/rand-inner
  (lambda (p k)
    `(rator/rand-inner ,p ,k)))

(define extend-env
  (lambda (id val env)
    `(extend-env ,id ,val ,env)))

(define base-env
  (lambda ()
    '()))

(define empty-k
  (lambda ()
    `(empty-k)))

(define closure
  (lambda (id body env)
    `(closure ,id ,body ,env)))

(define closure/cc
  (lambda (k)
    `(closure/cc ,k)))

(test "fact-5 Step 2"
  (eval-expr fact-5)
  120)

(test "call/cc Step 2"
  (eval-expr call/cc-fun)
  60)
; /////////////////////////////////////////////////////////////////////////////
; Step 3: Registerize step 2

(define *rator 'rator)
(define *rand 'rand)
(define *expr 'expr)
(define *env 'env)
(define *pc 'pc)
(define *a 'a)
(define *x 'x)
(define *k 'k)

(define ee
  (lambda () ;;; expr env k
    (pmatch *expr
      (,n (or (number? n) (boolean? n)) (set! *k *k) (set! *a n)
       (set! *pc apply-k))
      (,x (symbol? x) (set! *k *k) (set! *x x) (set! *env *env)
       (set! *pc apply-env))
      ((* ,x1 ,x2) () (set! *k (*-outer x2 *env *k)) (set! *env *env)
       (set! *expr x1) (set! *pc ee))
      ((sub1 ,x) () (set! *k (sub1-outer *k)) (set! *env *env)
       (set! *expr x) (set! *pc ee))
      ((zero? ,x) () (set! *k (zero?-outer *k)) (set! *env *env)
       (set! *expr x) (set! *pc ee))
      ((if ,test ,conseq ,alt) () (set! *k (if-outer conseq alt *env *k))
       (set! *env *env) (set! *expr test) (set! *pc ee))
      ((call/cc ,rator) () (set! *k (call/cc-outer *k))
       (set! *env *env) (set! *expr rator) (set! *pc ee))
      ((lambda (,id) ,body) () (set! *k *k) (set! *a (closure id body *env))
       (set! *pc apply-k))
      ((,rator ,rand) () (set! *k (rator/rand-outer rand *env *k))
       (set! *env *env) (set! *expr rator) (set! *pc ee)))
    (*pc)))

(define eval-expr
  (lambda (expr)
    (set! *k (empty-k))
    (set! *env (base-env))
    (set! *expr expr)
    (set! *pc ee)
    (*pc)))

(define apply-proc
  (lambda () ;;; rator rand k
    (pmatch *rator
      [(closure ,id ,body ,env) () (set! *k *k)
       (set! *env (extend-env id *rand env)) (set! *expr body) (set! *pc ee)]
      [(closure/cc ,k) () (set! *k k) (set! *a *rand) (set! *pc apply-k)])
    (*pc)))

(define apply-env
  (lambda () ;;; env x k
    (pmatch *env
      [() () (error 'apply-env "Unbound variable ~s" x)]
      [(extend-env ,id ,val ,env) ()
       (set! *k *k)
       (if (eq? *x id)
           (begin
             (set! *a val)
             (set! *pc apply-k))
           (begin
             (set! *x *x)
             (set! *env env)
             (set! *pc apply-env)))])
    (*pc)))

(define apply-k
  (lambda () ;;; k a
    (pmatch *k
      [(rator/rand-inner ,p ,k) () (set! *k k) (set! *rand *a)
       (set! *rator p) (set! *pc apply-proc) (*pc)]
      [(rator/rand-outer ,rand ,env ,k) () (set! *k (rator/rand-inner *a k))
       (set! *env env) (set! *expr rand) (set! *pc ee) (*pc)]
      [(call/cc-outer ,k) () (set! *k k) (set! *rand (closure/cc k))
       (set! *rator *a) (set! *pc apply-proc) (*pc)]
      [(if-outer ,conseq ,alt ,env ,k) () (set! *k k) (set! *env env)
       (if *a
           (set! *expr conseq)
           (set! *expr alt))
       (set! *pc ee) (*pc)]
      [(zero?-outer ,k) () (set! *k k) (set! *a (zero? *a))
       (set! *pc apply-k) (*pc)]
      [(sub1-outer ,k) () (set! *k k) (set! *a (sub1 *a))
       (set! *pc apply-k) (*pc)]
      [(*-inner ,v ,k) () (set! *k k) (set! *a (* v *a))
       (set! *pc apply-k) (*pc)]
      [(*-outer ,x2 ,env ,k) () (set! *k (*-inner *a k))
       (set! *env env) (set! *expr x2) (set! *pc ee) (*pc)]
      [(empty-k) () *a])))

(define *-inner
  (lambda (v k)
    `(*-inner ,v ,k)))

(define *-outer
  (lambda (x2 env k)
    `(*-outer ,x2 ,env ,k)))

(define sub1-outer
  (lambda (k)
    `(sub1-outer ,k)))

(define zero?-outer
  (lambda (k)
    `(zero?-outer ,k)))

(define if-outer
  (lambda (conseq alt env k)
    `(if-outer ,conseq ,alt ,env ,k)))

(define call/cc-outer
  (lambda (k)
    `(call/cc-outer ,k)))

(define rator/rand-outer
  (lambda (rand env k)
    `(rator/rand-outer ,rand ,env ,k)))

(define rator/rand-inner
  (lambda (p k)
    `(rator/rand-inner ,p ,k)))

(define extend-env
  (lambda (id val env)
    `(extend-env ,id ,val ,env)))

(define base-env
  (lambda ()
    '()))

(define empty-k
  (lambda ()
    `(empty-k)))

(define closure
  (lambda (id body env)
    `(closure ,id ,body ,env)))

(define closure/cc
  (lambda (k)
    `(closure/cc ,k)))

(test "fact-5 Step 3"
  (eval-expr fact-5)
  120)

(test "call/cc Step 3"
  (eval-expr call/cc-fun)
  60)
; /////////////////////////////////////////////////////////////////////////////
; Step 4: Trample step 3

(define *rator 'rator)
(define *rand 'rand)
(define *expr 'expr)
(define *env 'env)
(define *pc 'pc)
(define *a 'a)
(define *x 'x)
(define *k 'k)

(define ee
  (lambda () ;;; expr env k
    (pmatch *expr
      (,n (or (number? n) (boolean? n)) (set! *k *k) (set! *a n)
       (set! *pc apply-k))
      (,x (symbol? x) (set! *k *k) (set! *x x) (set! *env *env)
       (set! *pc apply-env))
      ((* ,x1 ,x2) () (set! *k (*-outer x2 *env *k)) (set! *env *env)
       (set! *expr x1) (set! *pc ee))
      ((sub1 ,x) () (set! *k (sub1-outer *k)) (set! *env *env)
       (set! *expr x) (set! *pc ee))
      ((zero? ,x) () (set! *k (zero?-outer *k)) (set! *env *env)
       (set! *expr x) (set! *pc ee))
      ((if ,test ,conseq ,alt) () (set! *k (if-outer conseq alt *env *k))
       (set! *env *env) (set! *expr test) (set! *pc ee))
      ((call/cc ,rator) () (set! *k (call/cc-outer *k))
       (set! *env *env) (set! *expr rator) (set! *pc ee))
      ((lambda (,id) ,body) () (set! *k *k) (set! *a (closure id body *env))
       (set! *pc apply-k))
      ((,rator ,rand) () (set! *k (rator/rand-outer rand *env *k)) (set! *env *env)
       (set! *expr rator) (set! *pc ee)))))

(define tramp
  (lambda ()
    (*pc)
    (tramp)))

(define eval-expr
  (lambda (expr)
    (call/cc (lambda (jumpout)
               (set! *k (empty-k jumpout))
               (set! *env (base-env))
               (set! *expr expr)
               (set! *pc ee)
               (tramp)))))

(define apply-proc
  (lambda () ;;; rator rand k
    (pmatch *rator
      [(closure ,id ,body ,env) () (set! *k *k)
       (set! *env (extend-env id *rand env)) (set! *expr body) (set! *pc ee)]
      [(closure/cc ,k) () (set! *k k) (set! *a *rand) (set! *pc apply-k)])))

(define apply-env
  (lambda () ;;; env x k
    (pmatch *env
      [() () (error 'apply-env "Unbound variable ~s" x)]
      [(extend-env ,id ,val ,env) ()
       (set! *k *k)
       (if (eq? *x id)
           (begin
             (set! *a val)
             (set! *pc apply-k))
           (begin
             (set! *x *x)
             (set! *env env)
             (set! *pc apply-env)))])))
  
(define apply-k
  (lambda () ;;; k a
    (pmatch *k
      [(rator/rand-inner ,p ,k) () (set! *k k) (set! *rand *a)
       (set! *rator p) (set! *pc apply-proc)]
      [(rator/rand-outer ,rand ,env ,k) () (set! *k (rator/rand-inner *a k)) 
       (set! *env env) (set! *expr rand) (set! *pc ee)]
      [(call/cc-outer ,k) () (set! *k k) (set! *rand (closure/cc k))
       (set! *rator *a) (set! *pc apply-proc)]
      [(if-outer ,conseq ,alt ,env ,k) () (set! *k k) (set! *env env)
       (if *a
           (set! *expr conseq)
           (set! *expr alt))
       (set! *pc ee)]
      [(zero?-outer ,k) () (set! *k k) (set! *a (zero? *a)) (set! *pc apply-k)]
      [(sub1-outer ,k) () (set! *k k) (set! *a (sub1 *a)) (set! *pc apply-k)]
      [(*-inner ,v ,k) () (set! *k k) (set! *a (* v *a)) (set! *pc apply-k)]
      [(*-outer ,x2 ,env ,k) () (set! *k (*-inner *a k))
       (set! *env env) (set! *expr x2) (set! *pc ee)]
      [(empty-k ,jumpout) () (jumpout *a)])))

(define *-inner
  (lambda (v k)
    `(*-inner ,v ,k)))

(define *-outer
  (lambda (x2 env k)
    `(*-outer ,x2 ,env ,k)))

(define sub1-outer
  (lambda (k)
    `(sub1-outer ,k)))

(define zero?-outer
  (lambda (k)
    `(zero?-outer ,k)))

(define if-outer
  (lambda (conseq alt env k)
    `(if-outer ,conseq ,alt ,env ,k)))

(define call/cc-outer
  (lambda (k)
    `(call/cc-outer ,k)))

(define rator/rand-outer
  (lambda (rand env k)
    `(rator/rand-outer ,rand ,env ,k)))

(define rator/rand-inner
  (lambda (p k)
    `(rator/rand-inner ,p ,k)))

(define extend-env
  (lambda (id val env)
    `(extend-env ,id ,val ,env)))

(define base-env
  (lambda ()
    '()))

(define empty-k
  (lambda (jumpout)
    `(empty-k ,jumpout)))

(define closure
  (lambda (id body env)
    `(closure ,id ,body ,env)))

(define closure/cc
  (lambda (k)
    `(closure/cc ,k)))

(test "fact-5 Step 4"
  (eval-expr fact-5)
  120)

(test "call/cc Step 4"
  (eval-expr call/cc-fun)
  60)
; /////////////////////////////////////////////////////////////////////////////

; BONUS!

; CPS, Trampled

(define ee
  (lambda (expr env k)
    (lambda ()
      (pmatch expr
        (,n (or (number? n) (boolean? n)) (apply-k k n))
        (,x (symbol? x) (apply-env env x k))
        ((* ,x1 ,x2) () (ee x1 env (*-outer x2 env k)))
        ((sub1 ,x) () (ee x env (sub1-outer k)))
        ((zero? ,x) () (ee x env (zero?-outer k)))
        ((if ,test ,conseq ,alt) () (ee test env (if-outer conseq alt env k)))
        ((call/cc ,rator) () (ee rator env (call/cc-outer k)))
        ((lambda (,id) ,body) () (apply-k k (closure id body env)))
        ((,rator ,rand) () (ee rator env (rator/rand-outer rand env k)))))))

(define eval-expr
  (lambda (expr)
    (call/cc (lambda (jumpout)
               (tramp (ee expr (base-env) (empty-k jumpout)))))))

(define tramp
  (lambda (th)
    (tramp (th))))

(define apply-proc
  (lambda (rator rand k)
    (lambda ()
      (pmatch rator
        [(closure ,id ,body ,env) () (ee body (extend-env id rand env) k)]
        [(closure/cc ,k) () (apply-k k rand)]))))

(define apply-env
  (lambda (env x k)
    (lambda ()
      (pmatch env
        [() () (error 'apply-env "Unbound variable ~s" x)]
        [(extend-env ,id ,val ,env) () (if (eq? x id)
                                           (apply-k k val)
                                           (apply-env env x k))]))))

(define apply-k
  (lambda (k a)
    (lambda ()
      (pmatch k
        [(rator/rand-inner ,p ,k) () (apply-proc p a k)]
        [(rator/rand-outer ,rand ,env ,k) () (ee rand env (rator/rand-inner a k))]
        [(call/cc-outer ,k) () (apply-proc a (closure/cc k) k)]
        [(if-outer ,conseq ,alt ,env ,k) () (if a
                                                (ee conseq env k)
                                                (ee alt env k))]
        [(zero?-outer ,k) () (apply-k k (zero? a))]
        [(sub1-outer ,k) () (apply-k k (sub1 a))]
        [(*-inner ,v ,k) () (apply-k k (* v a))]
        [(*-outer ,x2 ,env ,k) () (ee x2 env (*-inner a k))]
        [(empty-k ,jumpout) () (jumpout a)]))))

(define *-inner
  (lambda (v k)
    `(*-inner ,v ,k)))

(define *-outer
  (lambda (x2 env k)
    `(*-outer ,x2 ,env ,k)))

(define sub1-outer
  (lambda (k)
    `(sub1-outer ,k)))

(define zero?-outer
  (lambda (k)
    `(zero?-outer ,k)))

(define if-outer
  (lambda (conseq alt env k)
    `(if-outer ,conseq ,alt ,env ,k)))

(define call/cc-outer
  (lambda (k)
    `(call/cc-outer ,k)))

(define rator/rand-outer
  (lambda (rand env k)
    `(rator/rand-outer ,rand ,env ,k)))

(define rator/rand-inner
  (lambda (p k)
    `(rator/rand-inner ,p ,k)))

(define extend-env
  (lambda (id val env)
    `(extend-env ,id ,val ,env)))

(define base-env
  (lambda ()
    '()))

(define empty-k
  (lambda (jumpout)
    `(empty-k ,jumpout)))

(define closure
  (lambda (id body env)
    `(closure ,id ,body ,env)))

(define closure/cc
  (lambda (k)
    `(closure/cc ,k)))

(test "fact-5 BONUS"
  (eval-expr fact-5)
  120)

(test "call/cc BONUS"
  (eval-expr call/cc-fun)
  60)

; /////////////////////////////////////////////////////////////////////////////

; BONUS!

; CPS, Trampled, Registerized

(define *rator 'rator)
(define *rand 'rand)
(define *expr 'expr)
(define *env 'env)
(define *pc 'pc)
(define *th 'th)
(define *a 'a)
(define *x 'x)
(define *k 'k)

(define ee
  (lambda () ;;; expr env k
    (lambda ()
      (pmatch *expr
        (,n (or (number? n) (boolean? n)) (set! *a n) (set! *pc apply-k))
        (,x (symbol? x) (set! *x x) (set! *pc apply-env))
        ((* ,x1 ,x2) () (set! *k (*-outer x2 *env *k)) (set! *expr x1)
         (set! *pc ee))
        ((sub1 ,x) () (set! *k (sub1-outer *k)) (set! *expr x)
         (set! *pc ee))
        ((zero? ,x) () (set! *k (zero?-outer *k)) (set! *expr x)
         (set! *pc ee))
        ((if ,test ,conseq ,alt) () (set! *k (if-outer conseq alt *env *k))
         (set! *expr test) (set! *pc ee))
        ((call/cc ,rator) () (set! *k (call/cc-outer *k))
         (set! *expr rator) (set! *pc ee))
        ((lambda (,id) ,body) () (set! *a (closure id body *env))
         (set! *pc apply-k))
        ((,rator ,rand) () (set! *k (rator/rand-outer rand *env *k))
         (set! *expr rator) (set! *pc ee))))))

(define eval-expr
  (lambda (expr)
    (call/cc (lambda (jumpout)
               (set! *k (empty-k jumpout))
               (set! *env (base-env))
               (set! *expr expr)
               (set! *th ee)
                 (tramp)))))

(define tramp
  (lambda () ;;; th
    (begin
      ((*th))
      (set! *th *pc)
      (tramp)
      )))

(define apply-proc
  (lambda () ;;; rator rand k
    (lambda ()
      (pmatch *rator
        [(closure ,id ,body ,env) () (set! *k *k)
         (set! *env (extend-env id *rand env)) (set! *expr body) (set! *pc ee)]
        [(closure/cc ,k) () (set! *k k) (set! *a *rand) (set! *pc apply-k)]))))
  
(define apply-env
  (lambda () ;;; env x k
    (lambda ()
      (pmatch *env
        [() () (error 'apply-env "Unbound variable ~s" x)]
        [(extend-env ,id ,val ,env) ()
         (set! *k *k)
         (if (eq? *x id)
             (begin
               (set! *a val)
               (set! *pc apply-k))
             (begin
               (set! *x *x)
               (set! *env env)
               (set! *pc apply-env)))]))))
  
(define apply-k
  (lambda () ;;; k a
    (lambda ()
      (pmatch *k
        [(rator/rand-inner ,p ,k) () (set! *k k) (set! *rand *a)
         (set! *rator p) (set! *pc apply-proc)]
        [(rator/rand-outer ,rand ,env ,k) () (set! *k (rator/rand-inner *a k)) 
         (set! *env env) (set! *expr rand) (set! *pc ee)]
        [(call/cc-outer ,k) () (set! *k k) (set! *rand (closure/cc k))
         (set! *rator *a) (set! *pc apply-proc)]
        [(if-outer ,conseq ,alt ,env ,k) () (set! *k k) (set! *env env)
         (if *a
             (set! *expr conseq)
             (set! *expr alt))
         (set! *pc ee)]
        [(zero?-outer ,k) () (set! *k k) (set! *a (zero? *a)) (set! *pc apply-k)]
        [(sub1-outer ,k) () (set! *k k) (set! *a (sub1 *a)) (set! *pc apply-k)]
        [(*-inner ,v ,k) () (set! *k k) (set! *a (* v *a)) (set! *pc apply-k)]
        [(*-outer ,x2 ,env ,k) () (set! *k (*-inner *a k))
         (set! *env env) (set! *expr x2) (set! *pc ee)]
        [(empty-k ,jumpout) () (jumpout *a)]))))

(define *-inner
  (lambda (v k)
    `(*-inner ,v ,k)))

(define *-outer
  (lambda (x2 env k)
    `(*-outer ,x2 ,env ,k)))

(define sub1-outer
  (lambda (k)
    `(sub1-outer ,k)))

(define zero?-outer
  (lambda (k)
    `(zero?-outer ,k)))

(define if-outer
  (lambda (conseq alt env k)
    `(if-outer ,conseq ,alt ,env ,k)))

(define call/cc-outer
  (lambda (k)
    `(call/cc-outer ,k)))

(define rator/rand-outer
  (lambda (rand env k)
    `(rator/rand-outer ,rand ,env ,k)))

(define rator/rand-inner
  (lambda (p k)
    `(rator/rand-inner ,p ,k)))

(define extend-env
  (lambda (id val env)
    `(extend-env ,id ,val ,env)))

(define base-env
  (lambda ()
    '()))

(define empty-k
  (lambda (jumpout)
    `(empty-k ,jumpout)))

(define closure
  (lambda (id body env)
    `(closure ,id ,body ,env)))

(define closure/cc
  (lambda (k)
    `(closure/cc ,k)))

(test "fact-5 BONUS-reg"
  (eval-expr fact-5)
  120)

(test "call/cc BONUS-reg"
  (eval-expr call/cc-fun)
  60)