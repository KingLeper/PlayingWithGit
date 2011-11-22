;;; Code written by Oleg Kiselyov
;;; (http://pobox.com/~oleg/ftp/)
;;;
;;; Taken from leanTAP.scm
;;; http://kanren.cvs.sourceforge.net/kanren/kanren/mini/leanTAP.scm?view=log

; A simple linear pattern matcher
; It is efficient (generates code at macro-expansion time) and simple:
; it should work on any R5RS Scheme system.

; (pmatch exp <clause> ...[<else-clause>])
; <clause> ::= (<pattern> <guard> exp ...)
; <else-clause> ::= (else exp ...)
; <guard> ::= boolean exp | ()
; <pattern> :: =
;        ,var  -- matches always and binds the var
;                 pattern must be linear! No check is done
;         _    -- matches always
;        'exp  -- comparison with exp (using equal?)
;        exp   -- comparison with exp (using equal?)
;        (<pattern1> <pattern2> ...) -- matches the list of patterns
;        (<pattern1> . <pattern2>)  -- ditto
;        ()    -- matches the empty list

(define-syntax pmatch
  (syntax-rules ()
    ((_ exp clause ...)
     (let ((val-to-match exp))
       (pmatch* val-to-match clause ...)))))

(define match-failure
  (lambda (val)
    (error 'match-failure "failed match ~s\n" val)))

(define-syntax pmatch*
  (syntax-rules (else)
    ((_ val (else exp ...))
     (let () exp ...))
    ((_ val)
     (match-failure val))
    ((_ val (pattern () exp0 exp ...) . clauses)
     (let ((fail (lambda () (pmatch* val . clauses))))
                                        ; note that match-pattern may do binding. Here,
                                        ; other clauses are outside of these binding
       (match-pattern val pattern (let () exp0 exp ...) (fail))))
    ((_ val (pattern guard exp0 exp ...) . clauses)
     (let ((fail (lambda () (pmatch* val . clauses))))
       (match-pattern val pattern
                      (if guard (let () exp0 exp ...) (fail))
                      (fail))))))

; (match-pattern val pattern kt kf)
(define-syntax match-pattern
  (syntax-rules (_ quote unquote)
    ((_ val _ kt kf) kt)
    ((_ val () kt kf)
     (if (null? val) kt kf))
    ((_ val (quote lit) kt kf)
     (if (equal? val (quote lit)) kt kf))
    ((_ val (unquote var) kt kf)
     (let ((var val)) kt))
    ((_ val (x . y) kt kf)
     (if (pair? val)
         (let ((valx (car val))
               (valy (cdr val)))
           (match-pattern valx x
                          (match-pattern valy y kt kf)
                          kf))
         kf))
    ((_ val lit kt kf)
     (if (equal? val (quote lit)) kt kf))))
