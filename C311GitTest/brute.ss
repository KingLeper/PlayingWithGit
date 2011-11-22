(define brute
  (lambda (n)
    (cond
      [(zero? (+(- (* n n) (* 10 n)) 24)) n]
      [else (brute (sub1 n))])))


(define enumerate
  (lambda (n)
    (cond
      [(eq? n 25) '()]
      [else (cons (cons n (+ (- (* n n) (* 12 n)) 32))(enumerate(add1 n)))])))

(define half-life
  (lambda (n x)
    (cond
      [(eq? n 100) x]
      [else (half-life (add1 n) (/ x 2))])))

(define assinine
  (lambda (x)
    (/ (- (sqrt (+ x 25)) 5) x)))

(define limit9
  (lambda (x)
    (/ (- (/ 1 x) (/ 1 4)) (- x 4))))

(define limit10
  (lambda (x)
    (/ (* 18 x) (expt (+ (expt x 4) 1) .25))))

(define derivitive
  (lambda (x)
    (/ (-(expt x 5) (expt 4 5)) (- x 4))))