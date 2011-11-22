;Alex Manus
;Assignment 6
;10/03/06


(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (let* ((expected expected-result)
              (produced tested-expression))
         (if (equal? expected produced)
             (printf "~s works!\n" title)
             (error 'test-check
                    "Failed ~s: ~a~%Expected: ~a~%Computed: ~a~%"
                     title 'tested-expression expected produced)))))))

(define trees-tests
  (lambda ()
    (test-check "trees1"
      (tree-sum '(1 2 3 4))
      10)
    (test-check "trees2"
      (tree-sum '(5))
      5)
    (test-check "trees3"
      (tree-sum '(1 22 333 4444))
      4800)
    (test-check "trees4"
      (tree-sum '(0 5 9))
      14)
    (test-check "trees5"
      (tree-sum '(65 2 91))
      158)))

(define walk-tests
  (lambda ()
    (test-check "walk1"
      (walk 'j '((p 78) (j 97) (l 54)))
      '(97))
    (test-check "walk2"
      (walk 'l '((p 78) (j 97) (l 54)))
      '(54))
    (test-check "walk3"
      (walk 'p '((p 78) (j 97) (l 54)))
      '(78))
    (test-check "walk4"
      (walk 'j '())
      'j)
    (test-check "walk5"
      (walk 'r '((p 78) (j 97) (l 54)))
      'r)))
    
(define depth-tests
  (lambda ()
    (test-check "depth1"
      (depth '(((((5))))))
      5)
    (test-check "depth2"
      (depth '(((((5)))(6))))
      5)
    (test-check "depth3"
      (depth '(((9)(((5))))))
      5)
    (test-check "depth4"
      (depth '((((4)))))
      4)
    (test-check "depth5"
      (depth '())
      1)))


(define ack-tests
  (lambda ()
    (test-check "ack1"
      (ack 2 3)
      9)
    (test-check "ack2"
      (ack 3 2)
      29)
    (test-check "ack3"
      (ack 0 3)
      4)
    (test-check "ack4"
      (ack 1 4)
      6)
    (test-check "ack5"
      (ack 3 1)
      13)))


(define fact-tests
  (lambda ()
    (test-check "fact1"
      (fact 0)
      1)
    (test-check "fact2"
      (fact 1)
      1)
    (test-check "fact3"
      (fact 2)
      2)
    (test-check "fact4"
      (fact 3)
      6)
    (test-check "fact5"
      (fact 10)
      3628800)))
      
(define pascal-tests
  (lambda ()
    (test-check "pascal1"
      (pascal 0)
      '())
    (test-check "pascal2"
      (pascal 1)
      '(1))
    (test-check "pascal3"
      (pascal 2)
      '(1 3))
    (test-check "pascal4"
      (pascal 3)
      '(1 3 6))
    (test-check "pascal5"
      (pascal 4)
      '(1 3 6 10))))

(define empty-kt 
  (lambda ()
    (let ([okay #t])
      (lambda (v) v))))

   
(define empty-k 
  (lambda ()
    (let ([okay #t])
      (lambda (v)
        (if okay
            (begin
              (set! okay #f)
              v)
            (error 'mt-k "k invoked in non-tail position"))))))

(define tree-sum
  (lambda (ls)
  (tree-sum-k ls (empty-k))))
    
(define walk
  (lambda (v ls)
    (walk-k v ls (empty-k))))

    
(define depth
  (lambda (ls)
   (depth-k ls (empty-k))))

(define ack
  (lambda (n m)
    (ack-k n m (empty-k))))

(define fact
  (lambda (n)
    (fact-k n (empty-k))))
      
(define pascal
  (lambda (n)
    (pascal-k n (empty-k))))

(define tree-sum-k
  (lambda (ls k)
    (cond
      [(null? ls) (k 0)]
      [(pair? (car ls))
       (tree-sum-k (car ls) (lambda (v)
                              (tree-sum-k (cdr ls)) (lambda (w)
                                                      (k (+ v w)))))]
      [else (tree-sum-k (cdr ls) (lambda (v)
                                   (k (+ v (car ls)))))])))

(define walk-k
  (lambda (v ls k)
    (cond
      [(symbol? v)
       (let ((p (assq v ls)))
         (cond
           [p (walk-k (cdr p) ls k)]
           [else (k v)]))]
      [else (k v)])))

(define depth-k
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(pair? (car ls))
       (let ((l (depth-k (car ls) (lambda (v)
                                    (k(add1 v)))))
             (r (depth-k (cdr ls) k)))
         (if (< l r) r l))]
      [else (depth-k (cdr ls) k)])))


(define ack-k
  (lambda (n m k)
    (cond
      [(zero? n) (k (add1 m))]
      [(zero? m) (ack-k (sub1 n) 1 k)]
      [else (ack-k n (sub1 m) (lambda (v)
                                (ack-k (sub1 n) v k)))])))

(define fib-cps
  (lambda (n k)
    (cond
      [(zero? n) (k 1)]
      [(= n 1) (k 1)]
      [else (fib-cps (sub1 n) (lambda (v)
                                (fib-cps (sub1 (sub1 n)) (lambda (w)
                                                           (k (+ v w))))))])))

(define fact-k
  (lambda (n k)
    (cond
      [(zero? n) (k 1)]
      [else (fact-k (sub1 n) (lambda (v)
                                 (k (* n v))))])))

(define pascal-k
  (lambda (n k)
    (let ((pascal
           (lambda (pascal k)
             (lambda (m a)
               (cond
                 [(> m n) (k '())]
                 [else (let ((a (+ a m)))
                         ((pascal pascal (lambda (v)
                                           (k (cons a v)))) (add1 m) a))])))))
      ((pascal pascal k) 1 0))))

(define pascal-k
  (lambda (n k)
    (let ((pascal
           (lambda (pascal k)
             (k (lambda (m a k)
               (cond
                 [(> m n) (k '())]
                 [else (let ((a (+ a m)))
                         ((pascal pascal (lambda (f)
                                           (f (add1 m) a)
                                           (lambda (v)
                                             (k(cons a v)))))))]
      ((pascal pascal (lambda (f)
                        (f 1 0 k)))))))))))))


(fact-tests)
(trees-tests)
(walk-tests)
(ack-tests)
(pascal-tests)
(depth-tests)