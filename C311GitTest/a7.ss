;Alex Manus
;10/11/06
;C311
;Assignment 7


;1
(trace-define ee
  (lambda (expr env k)
    (pmatch expr
      [,n (or (number? n) (boolean? n)) (k n)]
      [,id (symbol? id) (apply-env env id k)]
      [(* ,x1 ,x2) () (ee x1 env (lambda (v)
                                   (ee x2 env (lambda (w)
                                                (k (* v w))))))]
      [(sub1 ,x) () (ee x env (lambda (v)
                                 (k (sub1 v))))]
      [(zero? ,x) () (ee x env (lambda (v)
                                  (k (zero? v))))]
      [(if ,test ,conseq ,alt) ()
       (ee test env (lambda (v)
                      (if v (ee conseq env k)
                          (ee alt env k))))]
      [(call/cc ,rator) ()
       (call/cc
         (lambda (k)
           (apply-proc (ee rator env k) k)))]      
      [(lambda (,id) ,body) () (k (lambda (a k)
                                    (ee body
                                      (lambda (id* k*)
                                        (if (eq? id* id) (k* a)
                                            (apply-env env id* k*)))k)))]
      [(,rator ,rand) () (ee rator env (lambda (v)
                                         (ee rand env
                                         (lambda (w)
                                           (v w k)))))])))

;Helpers
;ENV: PROC
(define base-env
  (lambda ()
    (lambda (x)
      (error 'base-env "Unbound variable ~s" x))))

(define extend-env
  (lambda (id val env)
    (lambda (x)
      (if (eq? x id)
          val
          (apply-env env x empty-k)))))

(define apply-env
  (lambda (env id k)
    (env id)))

;CLOSURE: PROC
(define apply-proc
  (lambda (rator rand)
    (rator rand)))

(define make-closure
  (lambda (eval-expr)
    (lambda (id body env)
      (lambda (a)
        (eval-expr body (extend-env id a env))))))

;KONTINUATIONS: PROC
(define empty-k 
  (lambda ()
    (let ([okay #t])
      (lambda (v)
        (if okay
            (begin
              (set! okay #f)
              v)
            (error 'mt-k "k invoked in non-tail position"))))))


(define eval-expr
  (lambda (expr)
    (ee expr (base-env) (empty-k))))


(define-syntax test
  (syntax-rules ()
    [(_ name e1 e2)
     (let ([v1 e1][v2 e2])
       (if (equal? v1 v2)
           (printf "Test ~s passed.\n" name)
           (error 'test "\nTest ~s failed.\n~s => ~s\n~s => ~s" name 'e1 v1 'e2 v2)))]))

(define fact-5
  '((lambda (f)
      ((f f) 5))
    (lambda (f)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))

(test "fact-5"
  (eval-expr fact-5)
  120)


(define call/cc-fun
  '(* 3 (call/cc (lambda (q) (* 2 (q 4))))))

(test "call/cc"
  (eval-expr call/cc-fun)
  12)
























;END