;Alex Manus
;C311
;Assignment 13

;1
(define-syntax *begin
    (syntax-rules ()
      [(_) (void)]
      [(_ e) e]
      [(_ e1 e2 ...) (if (or e1 #t)
                          (*begin e2 ...))]))

;2
(define-syntax *unless
    (syntax-rules ()
      [(_) (void)]
      [(_ e1 e2 ...) (if (not e1)
                         (*begin e2 ...) (void))]))


;3
;I assume the error case is valide as inputting (lambda (n) ) results
;in a syntax error

(define-syntax plambda
    (syntax-rules ()
      [(_ e1) 
       (error "Invalid syntax" "plambda requires at least one exp. in body")]
      [(_ e1 e2 ...) (lambda n (*begin (pretty-print (car n)) e2 ...))]))



;4
(define-syntax tel* 
     (syntax-rules () 
       [(_ () b ...) ((lambda () (*begin b ...)))]
       [(_ ((x0 e0) (x1 e1) ...) b ...) (tel* ((x1 e1) ...)
                                      (let ((x0 e0)) b ...))]))

;BT
(define-syntax letr 
     (syntax-rules () 
       [(_ () b) ((lambda () (*begin b)))]
       [(_ ((x0 e0) (x1 e1) ...) b)
        (let ((x0 (lambda (x0 x1 ...) e0)))
          (letr ((x1 e1) ...) b))]))

(define-syntax test
  (syntax-rules (k j)
    [(_ b ...) (*begin b ...)]))


(letr ((even (lambda (n)
               (if (zero? n)
                   #t
                   (odd (sub1 n)))))
       (odd (lambda (n)
              (if (zero? n)
                  #f
                  (even (sub1 n))))))
      (odd 5))

(let ([even
       (lambda (even odd)
         (lambda (n)
           (let ([even (even even odd)]
                 [odd (odd even odd)])
             (if (zero? n)
                 #t
                 (odd (sub1 n))))))]
      [odd
       (lambda (even odd)
         (lambda (n)
           (let ([even (even even odd)]
                 [odd (odd even odd)])
             (if (zero? n)
                 #f
                 (even (sub1 n))))))])
  (let ([even (even even odd)]
        [odd (odd even odd)])
    (odd 5)))



;END