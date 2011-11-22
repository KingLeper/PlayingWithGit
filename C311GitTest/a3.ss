;Alex Manus
;C311
;9/12/06
;Assignment 3

;1
(define eval/proc/proc
  (lambda (exp env)
    (pmatch exp
      [,id (symbol? id) (apply-env/proc/proc env id)]
      [(lambda (, x), body) () (make-closure/proc/proc x body env)]
      [(+, n , m) () (+ (eval/proc/proc n env) (eval/proc/proc m env))]
      [(-, n ,m) () (- (eval/proc/proc n env) (eval/proc/proc m env))]
      [(*, n ,m) () (* (eval/proc/proc n env) (eval/proc/proc m env))]
      [(sub1, n) () (sub1 (eval/proc/proc n env))]
      [(if ,test ,then ,other) () (if (eval/proc/proc test env)
                                      (eval/proc/proc then env)
                                      (eval/proc/proc other env))]
      [(zero? ,n) () (zero? (eval/proc/proc n env))]
      [,n (integer? n) () n]
      [(,rator ,rand) () (apply-closure/proc/proc (eval/proc/proc rator env) 
                           (eval/proc/proc rand env))])))

(define base-env/proc/proc
  (lambda ()
    (lambda (r)
      (error 'unbound-id "~s" r))))
    
(define extend-env/proc/proc
  (lambda (r* val env)
    (lambda (r)
      (cond
        [(eq? r r*) v]
        [else (env r)]))))
    
(define apply-env/proc/proc
  (lambda (env r)
    (env r)))

(define make-closure/proc/proc
  (lambda (x body env)
    (lambda (a)
      (eval/proc/proc body
        (lambda (id)
          (if (eq? id x) a (env id)))))))

(define apply-closure/proc/proc
  (lambda (closure a)
    (closure a)))

;2
(define eval/proc/ds
  (lambda (exp env)
    (pmatch exp
      [,id (symbol? id) (apply-env/proc/ds env id)]
      [(lambda (, x), body) () (make-closure/proc/ds x body env)]
      [(+, n , m) () (+ (eval/proc/ds n env) (eval/proc/ds m env))]
      [(-, n ,m) () (- (eval/proc/ds n env) (eval/proc/ds m env))]
      [(*, n ,m) () (* (eval/proc/ds n env) (eval/proc/ds m env))]
      [(sub1, n) () (sub1 (eval/proc/ds n env))]
      [(if ,test ,then ,other) () (if (eval/proc/ds test env)
                                      (eval/proc/ds then env)
                                      (eval/proc/ds other env))]
      [(zero? ,n) () (zero? (eval/proc/ds n env))]
      [,n (integer? n) () n]
      [(,rator ,rand) () (apply-closure/proc/ds (eval/proc/ds rator env) 
                           (eval/proc/ds rand env))])))

(define base-env/proc/ds
  (lambda ()
      '()))
    
(define extend-env/proc/ds
  (lambda (r* val env)
    (cons (cons r* val) env)))
    
(define apply-env/proc/ds
  (lambda (env r)
    (cond
      [(null? env) 'empty-env]
      [(eq? (caar env) r) (cdar env)]
      [else (apply-env/proc/ds (cdr env) r)])))

(define make-closure/proc/ds
  (lambda (x body env)
    (lambda (a)
      (eval/proc/ds body (cons (cons x a) env)))))

(define apply-closure/proc/ds
  (lambda (closure a)
    (closure a)))

;3 I had to implement closures as a data structure in my own way
;what I copied from the board in class showed closure being defined as:
;(define closure
;   (lambda (x body env)
;     '(closure, x, body, env)
;     '(closure, x, body, env)))
;Obviously I copied it wrong as that doesn't even make sense

(define eval/ds/proc
  (lambda (exp env)
    (pmatch exp
      [,id (symbol? id) (apply-env/ds/proc env id)]
      [(lambda (, x), body) () (make-closure/ds/proc x body env)]
      [(+, n , m) () (+ (eval/ds/proc n env) (eval/ds/proc m env))]
      [(-, n ,m) () (- (eval/ds/proc n env) (eval/ds/proc m env))]
      [(*, n ,m) () (* (eval/ds/proc n env) (eval/ds/proc m env))]
      [(sub1, n) () (sub1 (eval/ds/proc n env))]
      [(if ,test ,then ,other) () (if (eval/ds/proc test env)
                                      (eval/ds/proc then env)
                                      (eval/ds/proc other env))]
      [(zero? ,n) () (zero? (eval/ds/proc n env))]
      [,n (integer? n) () n]
      [(,rator ,rand) () (apply-closure/ds/proc (eval/ds/proc rator env) 
                           (eval/ds/proc rand env))])))

(define base-env/ds/proc
  (lambda ()
    (lambda (r)
      (error 'unbound-id "~s" r))))
    
(define extend-env/ds/proc
  (lambda (r* val env)
    (lambda (r)
      (cond
        [(eq? r r*) v]
        [else (env r)]))))
    
(define apply-env/ds/proc
  (lambda (env r)
    (env r)))

(define make-closure/ds/proc
  (lambda (x body env)
    (list x body env)))

(define apply-closure/ds/proc
  (lambda (closure a)
    (eval/ds/proc (cadr closure) 
      (lambda (r)
        (cond
          [(eq? r (car closure)) a]
          [else (apply-env/ds/proc (caddr closure) r)])))))

;4
(define eval/ds/ds
  (lambda (exp env)
    (pmatch exp
      [,id (symbol? id) (apply-env/ds/ds env id)]
      [(lambda (, x), body) () (make-closure/ds/ds x body env)]
      [(+, n , m) () (+ (eval/ds/ds n env) (eval/ds/ds m env))]
      [(-, n ,m) () (- (eval/ds/ds n env) (eval/ds/ds m env))]
      [(*, n ,m) () (* (eval/ds/ds n env) (eval/ds/ds m env))]
      [(sub1, n) () (sub1 (eval/ds/ds n env))]
      [(if ,test ,then ,other) () (if (eval/ds/ds test env)
                                      (eval/ds/ds then env)
                                      (eval/ds/ds other env))]
      [(zero? ,n) () (zero? (eval/ds/ds n env))]
      [,n (integer? n) () n]
      [(,rator ,rand) () (apply-closure/ds/ds (eval/ds/ds rator env) 
                           (eval/ds/ds rand env))])))

(define base-env/ds/ds
  (lambda ()
      '()))
    
(define extend-env/ds/ds
  (lambda (r* val env)
    (cons (cons r* val) env)))
    
(define apply-env/ds/ds
  (lambda (env r)
    (cond
      [(null? env) (error 'unbound-id "~s" r)]
      [(eq? (caar env) r) (cdar env)]
      [else (apply-env/ds/ds (cdr env) r)])))

(define make-closure/ds/ds
  (lambda (x body env)
    (list x body env)))

(define apply-closure/ds/ds
  (lambda (closure a)
    (eval/ds/ds (cadr closure) 
      (cons (cons (car closure) a) (caddr closure)))))

;BT1
(trace-define eval-exp
  (lambda (exp env extend-env apply-env make-closure apply-closure)
    (pmatch exp
      [,id (symbol? id) (apply-env env id)]
      [(lambda (, x), body) () (make-closure x body extend-env apply-env make-closure apply-closure)]
      [(+, n , m) () (+ (eval-exp n env extend-env apply-env make-closure apply-closure)
                      (eval-exp m env extend-env apply-env make-closure apply-closure))]
      [(-, n ,m) () (- (eval-exp n env extend-env apply-env make-closure apply-closure) 
                     (eval-exp m env extend-env apply-env make-closure apply-closure))]
      [(*, n ,m) () (* (eval-exp n env extend-env apply-env make-closure apply-closure) 
                     (eval-exp m env extend-env apply-env make-closure apply-closure))]
      [(sub1, n) () (sub1 (eval-exp n env extend-env apply-env make-closure apply-closure))]
      [(if ,test ,then ,other) () (if (eval-exp test env extend-env apply-env make-closure apply-closure)
                                      (eval-exp then env extend-env apply-env make-closure apply-closure)
                                      (eval-exp other env extend-env apply-env make-closure apply-closure))]
      [(zero? ,n) () (zero? (eval-exp n env extend-env apply-env make-closure apply-closure))]
      [,n (integer? n) () n]
      [(,rator ,rand) () (apply-closure (eval-exp rator env extend-env apply-env make-closure apply-closure) 
                           (eval-exp rand env extend-env apply-env make-closure apply-closure))])))


(define base-env/ds
  (lambda ()
      '()))
    
(define extend-env/ds
  (lambda (r* val env)
    (cons (cons r* val) env)))
    
(define apply-env/ds
  (lambda (env r)
    (cond
      [(null? env) (error 'unbound-id "~s" r)]
      [(eq? (caar env) r) (cdar env)]
      [else (apply-env/ds (cdr env) r)])))

(define make-closure/ds
  (lambda (x body env)
    (list x body env)))

(define apply-closure/ds
  (lambda (closure a)
    (eval/ds/ds (cadr closure) 
      (cons (cons (car closure) a) (caddr closure)))))

(define base-env/proc
  (lambda ()
    (lambda (r)
      (error 'unbound-id "~s" r))))
    
(define extend-env/proc
  (lambda (r* val env)
    (lambda (r)
      (cond
        [(eq? r r*) v]
        [else (env r)]))))
    
(define apply-env/proc
  (lambda (env r)
    (env r)))

(trace-define make-closure/proc
  (lambda (x body env extend-env apply-env make-closure apply-closure)
      (eval-exp
        (lambda (id)
          (if (eq? id x) a 
              (apply-env env id))) env extend-env apply-env make-closure apply-closure)))

(define apply-closure/proc
  (lambda (closure a)
    (closure a)))


;END