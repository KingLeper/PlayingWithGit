;;; This is code developed by Oscar Waddell for B521 (Fall 2002)
;;; There has been a small improvement: quote and begin are always
;;; included in those expressions to be expanded.
(print-gensym #f)
(module ((expand-only))
  ; Bugs:
  ;  - may give unexpected results with quotes, binding forms, etc.
  ;  - uses system internals that it should not

 ; cleanup needed because uncprep hard-codes list of keywords
  (define cleanup ; not robust
    (lambda (x)
      (syntax-case x (top-level-value quote)
        [((top-level-value (quote id)) . rest)
         (symbol? #'id)
         (cons #'id (cleanup #'rest))]
        [(x ...) (map cleanup #'(x ...))]
        [(quote datum) `(quote ,#'datum)]
        [else x])))

  (define delete-all-but
    (lambda (skip)
      (lambda (x)
        (unless (memq x skip)
          (#%\#set-system-property-list! x '())))))

  (define expand-only
    (lambda (names datum)
      (let ([syms (oblist)])
        (let ([ps (map #%\#system-property-list syms)]
	      [gensym-status (print-gensym)])
          (dynamic-wind
            (lambda ()
              (for-each (delete-all-but
			  (cons 'begin (cons 'quote names)))
		syms)
	      (print-gensym #f))
            (lambda ()
	      (cleanup (sc-expand datum)))
            (lambda ()
	      (print-gensym gensym-status)
	      (for-each #%\#set-system-property-list! syms ps))))))))

