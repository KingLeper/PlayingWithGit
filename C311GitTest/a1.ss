;Alex Manus amanus
;8/31/06
;C311
;correct header !!

;1
(define insertr
  (lambda (x y l)
    (cond
      [(null? l) '()]
      [(eq? x (car l)) (cons (car l) (cons y (insertr x y (cdr l))))]
      [else (cons (car l) (insertr x y (cdr l)))])))

;2
(define last
  (lambda (l)
  (cond
    [(null? (cdr l)) (car l)]
    [else (last(cdr l))])))

;3
(define increment
  (lambda (l)
    (cond
      [(null? l) '()]
      [else (cons (add1 (car l)) (increment (cdr l)))])))

;4
(define remove
  (lambda (x l)
    (cond
      [(null? l) '()]
      [(eq? x (car l)) (remove x (cdr l))]
      [else (cons (car l) (remove x (cdr l)))])))

;5
(define count-between
  (lambda (m n)
    (cond
      [(eq? m n) '()]
      [else (cons m (count-between (add1 m) n))])))

;6
(define zip
  (lambda (l1 l2)
    (cond
      [(null? l1) '()]
      [else (cons (cons (car l1) (cons (car l2) '())) (zip (cdr l1) (cdr l2)))])))



;7

(define occurs
  (lambda (x l)
    (cond
      [(null? l) 0]
      [(eq? x (car l)) (add1 (occurs x (cdr l)))]
      [else (occurs x (cdr l))])))

;8
(define filter
  (lambda (p l)
    (cond
      [(null? l) '()]
      [else (cons (p (car l)) (filter p (cdr l)))])))


;9
(define sum-to
  (lambda (i)
        (cond
          [(eq? 0 i) 0]
          [(<= i -1) (+ i (sum-to (add1 i)))]
          [else (+ i (sum-to (sub1 i)))])))



;10
(define mapit
  (lambda (p ls)
    (cond
      [(null? ls) '()]
      [else (cons (p (car ls)) (filter p (cdr ls)))])))

;11 is there a cleaner way to do this without adding a variable?
(define triple
  (lambda (l)
    (cond
      [(null? l) '()]
      [else (cons (car l) (cons (car l) (cons (car l) (triple (cdr l)))))])))

;12
(define append
  (lambda (l1 l2)
    (cond
      [(null? l1) l2]
      [else (cons (car l1) (append (cdr l1) l2))])))


;13
(define fact
  (lambda (n)
    (cond
      [(= 1 n) 1]
      [else (* n (fact (sub1 n)))])))

;14
(define fib
  (lambda (n)
    (cond
      [(<= n 1) n]
      [else (+ (fib(sub1 n)) (fib(- n 2)))])))


;15
(define even?
  (lambda (n)
    (cond
      [(zero? n) #t]
      [else (odd? (sub1 n))])))

(define odd?
  (lambda (n)
    (cond
      [(zero? n) #f]
      [else (even? (sub1 n))])))

;16
(define fib-acc
  (lambda (n x a)
    (cond
      [(and (<= n 2) (zero? a)) n]
      [(zero? n) a]
      [else (fib-acc (sub1 n) (add1 a) (+ a (sub1 x)))])))