;Alex Manus
;C311
;10/25/06
;With assistance from David Petro

;4
(define *rator 'rator)
(define *rand 'rand)
(define *expr 'expr)
(define *env 'env)
(define *pc 'pc)
(define *a 'a)
(define *x 'x)
(define *k 'k)

(define-union expr
  (n)
  (symbol )
  (* x1 x2))

(define ee
  (lambda () ;;; expr env k
    (union-case *expr exp
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

;END