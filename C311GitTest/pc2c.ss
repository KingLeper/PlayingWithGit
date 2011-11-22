(load "pmatch.scm")

(define reg-funcs '())

(define reg-pc 'no-pc)
(define dismount-var 'no-dis)
(define construct-var 'no-const)

(define add-func
  (lambda (func)
    (set! reg-funcs (cons func reg-funcs))))

(define is-func?
  (lambda (func)
    (assv func reg-funcs)))

(define reg-unions '())

(define add-union
  (lambda (union)
    (set! reg-unions (cons union reg-unions))))

(define reg-regs '())

(define init-storage
  (lambda ()
    (set! reg-funcs '())
    (set! reg-unions '())
    (set! reg-regs '())))


(define new-safe-char
  (lambda (char)
    (cond
      [(eq? #\? char) "r__q__"]
      [(eq? #\! char) "r__f__"]
      [(eq? #\. char) "r__d__"]
      [(eq? #\+ char) "r__p__"]
      [(eq? #\- char) "r__m__"]
      [(eq? #\* char) "r__t__"]
      [(eq? #\/ char) "r__di__"]
      [(eq? #\< char) "r__lt__"]
      [(eq? #\> char) "r__gt__"]
      [(eq? #\: char) "r__co__"]
      [(eq? #\$ char) "r__do__"]
      [(eq? #\% char) "r__pe__"]
      [(eq? #\^ char) "r__ex__"]
      [(eq? #\& char) "r__am__"]
      [(eq? #\~ char) "r__ti__"]
      [(eq? #\_ char) "r_"]
      [(and (char>=? char #\0) (char<=? char #\9))
       (string-append "r" (list->string `(,char)))]
      [else #f])))

(define safe 
  (lambda (sym)
    (if (symbol? sym)
     (let loop ([l (string->list (symbol->string sym))])
       (cond
         [(null? l) ""]
         [(new-safe-char (car l)) =>
          (lambda (x)
            (string-append x (loop (cdr l))))]
         [else (string-append (list->string `(,(car l)))
                              (loop (cdr l)))]))
        sym)))

(define join
  (lambda (lst separater)
    (let loop ([lst lst]
               [result ""]
               [is-first? #t])
      (cond
        [(null? lst) result]
        [is-first? (loop (cdr lst) (format "~a" (car lst)) #f)]
        [else (loop (cdr lst) (string-append result
                                (format "~a~a" separater (car lst))) #f)]))))


(define file->list
  (lambda (fname)
    (let ([file (open-input-file fname)])
      (let ([data
             (let recurse ([decl (read file)])
               (if (eof-object? decl)
                 '()
                 (cons decl (recurse (read file)))))])
        (close-input-port file)                                                         data))))


(define pc2c
  (lambda (file-name source-name header-name)
    ;; WARNING: pc2c will erase existing files when generating new ones!
    (delete-file source-name)
    (delete-file header-name)
    (init-storage)
    (let ([decl* (file->list file-name)])
      (let ([src (open-output-file source-name)]
            [hdr (open-output-file header-name)])

        ;; write a generated header file to header-name
        (display (pc2c-header decl*) hdr)

        (check-correct-info)

        ;; write a generated source file source-name
        (display (pc2c-source header-name) src)

        (close-output-port src)
        (close-output-port hdr)))))


(define check-correct-info
  (lambda ()
    (begin
      (if (null? reg-regs)
          (display "Warning: you have defined no registers.\n")))))


(define pc2c-append
  (lambda args
    (apply string-append
           (map (lambda (elt)
                  (cond
                    [(symbol? elt) (format "~a" elt)]
                    [(number? elt) (format "~s" elt)]
                    [(string? elt) elt]
                    [else (error 'pc2c-append "Invalid argument ~s" elt)]))
                args))))

(define pc2c-gen-unions
  (lambda (union)
    (let ([name (safe (car union))]
          [tag* (cadr union)]
          [field** (caddr union)])
      (apply string-append
             (map (lambda (tag field*)
                    (let ([tag (safe tag)])
                      (pc2c-append
                        (pc2c-fn-proto (pc2c-append name "r_" tag) field*) " {\n"
                        name "* _data = (" name "*)malloc(sizeof(" name "));\n"
                        "if(!_data) {\n"
                        "  fprintf(stderr, \"Out of memory\\n\");\n"
                        "  exit(1);\n"
                        "}\n"
                        "  _data->tag = _" tag "_" name ";\n"
                        (apply string-append
                               (map (lambda (field)
                                      (let ([field (safe field)])
                                        (format "  _data->u._~a._~a = ~a;\n"
                                                tag field field)))
                                    field*))
                        "  return (void *)_data;\n"
                        "}\n\n")))
                    tag* field**)))))


(define handle-union-case-case
  (lambda (name)
    (lambda (template body)
      (pmatch template
        [(,tag . ,var*) (list? var*)
         (let ([name (safe name)]
               [tag (safe tag)])
           (pc2c-append
            "case _" tag "_" name ": {\n"
            (apply string-append
                   (map (lambda (x)
                          (let ([x (safe x)])
                            (pc2c-append
                             "void *" x " = _c->u._" tag "._" x ";\n")))
                        var*))
            body
            "break; }\n"))]
        [else (string-append "default {\n"
                             body
                             "}\n")]))))



(define get-last
  (lambda (ls)
    (cond
      ((null? ls) #f)
      ((null? (cdr ls)) (car ls))
      (else (get-last (cdr ls))))))

(define remove-last
  (lambda (ls)
    (pmatch ls
      [((else ,body)) () '()]
      [((,test ,body) . ,c*) () `((,test ,body) . ,(remove-last c*))])))

(define parse-function-body
  (lambda (tail)
    (if tail
        (lambda (expr)
          (pmatch expr
            [(error ,msg) () (pc2c-append
                              "fprintf(stderr, \"" msg "\");\n exit(1);\n")]
            [(if ,test ,conseq ,alt) ()
             (let ((test ((parse-function-body #f) test))
                   (conseq ((parse-function-body #t) conseq))
                   (alt ((parse-function-body #t) alt)))
               (pc2c-append
                "if(" test ") {\n"
                "  " conseq "\n"
                "} else {\n"
                "  " alt "\n"
                "}\n"))]
            [(cond (else ,body)) ()
             (let ((body ((parse-function-body #t) body)))
               body)]
            [(cond . ,c*) ()
             (let ((last (get-last c*))
                   (c* (remove-last c*)))
               (cond
                 [(eq? (car last) 'else)
                  (let* ((test0 ((parse-function-body #f) (caar c*)))
                         (body0 ((parse-function-body #t) (cadar c*)))
                         (test* (map (parse-function-body #f) (map car (cdr c*))))
                         (body* (map (parse-function-body #t) (map cadr (cdr c*))))
                         (body ((parse-function-body #t) (cadr last))))
                    (pc2c-append
                     "if(" test0 ") {\n"
                     "  " body0 "\n"
                     "}"
                     (apply string-append
                            (map (lambda (x y)
                                   (pc2c-append " else if(" x ") {\n"
                                                y
                                                "}\n"))
                                 test* body*))
                     " else {\n"
                     "  " body "\n"
                     "}\n"))]
                 [else
                  (let* ((test0 ((parse-function-body #f) (caar c*)))
                         (body0 ((parse-function-body #t) (cadar c*)))
                         (test* (map (parse-function-body #f) (map car (cdr c*))))
                         (body* (map (parse-function-body #t) (map cadr (cdr c*)))))
                    (pc2c-append
                     "if(" test0 ") {\n"
                     "  " body0 "\n"
                     "}"
                     (apply string-append
                            (map (lambda (x y)
                                   (pc2c-append "else if(" x ") {\n"
                                                y
                                                "}\n"))
                                 test* body*))))]))]
            [(begin . ,expr*) ()
             (apply string-append (map (parse-function-body #t) expr*))]
            [(set! ,var ,var1) (eq? var var1) ""]
            [(set! ,var ,val) ()
             (let ((val ((parse-function-body #f) val)))
               (if (equal? (safe var) reg-pc)
                   (pc2c-append (safe var) " = &" val ";\n")
                   (pc2c-append (safe var) " = (void *)" val ";\n")))]
            [(union-case ,val ,name . ,c*) ()
             (let ((template* (map car c*))
                   (body* (map (parse-function-body #t) (map cadr c*))))
               (let ([name (safe name)]
                     [val (safe val)])
                 (pc2c-append
                  name "* _c = (" name "*)" val ";\n"
                  "switch (_c->tag) {\n"
                  (apply string-append
                         (map (handle-union-case-case name) template* body*))
                  "}\n"
                  )))]
            [(let ,bind* ,body) ()
             (let ((lhs* (map car bind*))
                   (rhs* (map (parse-function-body #f) (map cadr bind*))))
               (pc2c-append
                "{\n"
                (apply string-append
                       (map (lambda (x y)
                              (pc2c-append
                               "void *" (safe x) " = (void *)" y ";\n"))
                            lhs* rhs*))
                body
                "}\n"))]
            [(printf ,str . ,parms*) ()
             (let ([str (list->string
                         (let loop ([str (string->list str)])
                           (if (null? str)
                               '()
                               (if (char=? (car str) #\~)
                                   (if (and (not (null? (cdr str)))
                                            (not (char=? (cadr str) #\ )))
                                       (cons #\% (cons #\d (loop (cddr str))))
                                       (cons #\% (loop (cdr str))))
                                   (cons (car str) (loop (cdr str)))))))]
                   [parms (map safe parms*)])
               (string-append
                "printf(" (join (cons (format "~s" str) parms)
                                ", (int)") ");"))]
            [(mount-trampoline ,construct ,dismount ,pc) ()
             (set! construct-var (safe construct))
             (set! dismount-var (safe dismount))
             (pc2c-append
              "mount_tram();\n")]
            [(dismount-trampoline ,dismount) ()
             (pc2c-append
              "_trstr *trstr = (_trstr *)" (safe dismount) ";\n"
              "longjmp(*trstr->jmpbuf, 1);\n"
              )]
            [(,func) (is-func? func)
             (pc2c-append reg-pc " = &" (safe func) ";\n")]
            [,elsee ()
             (let ((elsee ((parse-function-body #f) elsee)))
               (pc2c-append "return(" elsee ");\n"))]
            ))
        (lambda (expr)
          (pmatch expr
            [#t () (pc2c-append "(void *)" 1)]
            [#f () (pc2c-append "(void *)" 0)]
            [,x (symbol? x) (safe x)]
            [,x (integer? x) (pc2c-append "(void *)" x)]
            [(zero? ,x) ()
             (let ((x ((parse-function-body #f) x)))
               (pc2c-append "(" x " == 0)"))]
            [(and ,a ,b) ()
             (let ((a ((parse-function-body #f) a))
                   (b ((parse-function-body #f) b)))
               (pc2c-append "(" a " && " b ")"))]
            [(or ,a ,b) ()
             (let ((a ((parse-function-body #f) a))
                   (b ((parse-function-body #f) b)))
               (pc2c-append "(" a " || " b ")"))]
            [(not ,x) ()
             (let ((x ((parse-function-body #f) x)))
               (pc2c-append "(!" x ")"))]
            [(< ,a ,b) ()
             (let ((a ((parse-function-body #f) a))
                   (b ((parse-function-body #f) b)))
               (pc2c-append "(" a " < " b ")"))]
            [(> ,a ,b) ()
             (let ((a ((parse-function-body #f) a))
                   (b ((parse-function-body #f) b)))
               (pc2c-append "(" a " > " b ")"))]
            [(<= ,a ,b) ()
             (let ((a ((parse-function-body #f) a))
                   (b ((parse-function-body #f) b)))
               (pc2c-append "(" a " <= " b ")"))]
            [(>= ,a ,b) ()
             (let ((a ((parse-function-body #f) a))
                   (b ((parse-function-body #f) b)))
               (pc2c-append "(" a " >= " b ")"))]
            [(+ ,a ,b) ()
             (let ((a ((parse-function-body #f) a))
                   (b ((parse-function-body #f) b)))
               (pc2c-append "(void *)((int)" a " + (int)" b ")"))]
            [(* ,a ,b) ()
             (let ((a ((parse-function-body #f) a))
                   (b ((parse-function-body #f) b)))
               (pc2c-append "(void *)((int)" a " * (int)" b ")"))]
            [(- ,a ,b) ()
             (let ((a ((parse-function-body #f) a))
                   (b ((parse-function-body #f) b)))
               (pc2c-append "(void *)((int)" a " - (int)" b ")"))]
            [(/ ,a ,b) ()
             (let ((a ((parse-function-body #f) a))
                   (b ((parse-function-body #f) b)))
               (pc2c-append "(void *)((int)" a " / (int)" b ")"))]
            [(sub1 ,a) ()
             (let ((a ((parse-function-body #f) a)))
               (pc2c-append "(void *)((int)" a " - 1)"))]
            [(add1 ,a) ()
             (let ((a ((parse-function-body #f) a)))
               (pc2c-append "(void *)((int)" a " + 1)"))]
            [(random ,x) ()
             (let ((x ((parse-function-body #f) x)))
               (pc2c-append "(void *)(rand() % (int)" x ")"))]
            [(if ,test ,conseq ,alt) ()
             (let ((test ((parse-function-body #f) test))
                   (conseq ((parse-function-body #f) conseq))
                   (alt ((parse-function-body #f) alt)))
               (pc2c-append "(" test " ? " conseq " : " alt ")"))]
            [(,func . ,args*) (symbol? func)
             (let ((args* (map (parse-function-body #f) args*)))
               (pc2c-append
                (safe func) "(" (join args* ",") ")"))])))))

(define pc2c-gen-funcs
  (lambda (func)
    (let ([name (safe (car func))]
          [body (cadr func)])
      (pc2c-append
        (if (equal? name "main")
            "int "
            "void ") name "()\n"
        "{\n"
        ((parse-function-body #t) body)
        "}\n\n"
        ))))

(define pc2c-source
  (lambda (header-name)
    (let* ([s1 (apply string-append (map pc2c-gen-unions reg-unions))]
           [s2 (apply string-append (map pc2c-gen-funcs reg-funcs))])
      (let ([s3 (pc2c-append
                 "int mount_tram ()\n"
                 "{\n"
                 "srand (time (NULL));\n"
                 "jmp_buf jb;\n"
                 "_trstr trstr;\n"
                 "void *dismount;\n"
                 "int _status = setjmp(jb);\n"
                 "trstr.jmpbuf = &jb;\n"
                 "dismount = &trstr;\n"
                 "if(!_status) {\n"
                 dismount-var "= (void *)" construct-var "(dismount);\n"
                 "for(;;) {\n"
                 reg-pc "();\n"
                 "}\n"
                 "}\n"
                 "return 0;\n"
                 "}\n")])
        (string-append
         "#include <setjmp.h>\n"
         "#include <assert.h>\n"
         "#include <stdlib.h>\n" ;; for malloc
         "#include <stdio.h>\n"
         "#include \"" header-name  "\"\n"
         "\n"
         s1
         s2
         s3)))))


(define pc2c-header
  (lambda (decl*)
    (string-append
      (apply string-append
             (map pc2c-header-parse decl*))
      "int mount_tram();\n\n"
      "struct _trstr;\n"
      "typedef struct _trstr _trstr;\n"
      "struct _trstr {\n"
      "  jmp_buf *jmpbuf;\n"
      "  int value;\n"
      "};\n\n")))

(define pc2c-header-parse
  (lambda (decl)
    (pmatch decl
      [(load ,file . ,file*) () ""]
      [(exit) () ""]
      [(display ,anything . ,anything*) () ""]
      [(pretty-print ,anything . ,anything*) () ""]
      [(define-registers . ,reg*) ()
       (set! reg-regs reg*)
       (if (null? reg*)
           ""
           (string-append
             "void *"
             (join (map safe reg*) ", *")
             ";\n\n"))]
      [(define-program-counter ,pc) () (set! reg-pc (safe pc))
       (string-append "void (*" reg-pc ")();\n\n")]
      [(define-union ,name . ,c*) ()
       (let ((tag* (map car c*))
             (field** (map cdr c*)))
         (add-union `(,name ,tag* ,field**))
         (let ([name (safe name)])
           (let ([enum-tags
                  (join
                   (map (lambda (tag)
                          (pc2c-append "_" (safe tag) "_" name)) tag*)
                   ",\n    ")]
                 [structs
                  (apply string-append
                         (map
                          (lambda (tag field*)
                            (let ([tag (safe tag)])
                              (if (null? field*) 
                                  (format
                                   "    struct { char dummy; } _~a;\n" tag)
                                  (string-append
                                   "    struct {"
                                   (apply string-append
                                          (map
                                           (lambda (field)
                                             (format " void *_~a;" (safe field)))
                                           field*))
                                   (format " } _~a;\n" tag)))))
                          tag* field**))]
                 [structors
                  (apply string-append
                         (map
                          (lambda (tag field*)
                            (let ([tag (safe tag)])
                              (string-append
                               (pc2c-fn-proto (pc2c-append name "r_" tag)
                                              field*)
                               ";\n")))
                          tag* field**))])
             (pc2c-append
              "struct " name ";\n"
              "typedef struct " name " " name ";\n"
              "struct " name " {\n"
              "  enum {\n"
              "    " enum-tags "\n"
              "  } tag;\n"
              "  union {\n"
              structs
              "  } u;\n"
              "};\n\n"
              structors "\n"))))]
      [(define-label ,name ,body) ()
       (begin (add-func `(,name ,body))
              (string-append (if (equal? (safe name) "main")
                                 "int "
                                 "void ") (safe name) "();\n"))])))

(define pc2c-fn-proto
  (lambda (fn-name param*)
    (let ([declare-params
           (lambda (param*)
             (join (map (lambda (param)
                          (format "void *~a" (safe param))) param*) ", "))])
      (pc2c-append
        "void *" (safe fn-name) "(" (declare-params param*)  ")"))))

(define compile/run
  (lambda (base-name)
    (let ([pc-file (string-append base-name ".ss")]
                   [c-file (string-append base-name ".c")]
                   [header-file (string-append base-name ".h")])
      (pc2c pc-file c-file header-file)
      (let ([compile-command (string-append "gcc -o " base-name " " c-file)]
            [indent-command (string-append "indent -bad -bap -br -nbs -ce"
                                           "-cli1 -di1 -i2 -npro -npsl ")])
        (let ([compile-val (system compile-command)])
          (unless (zero? compile-val)
            (error 'compile/run "Compilation command '~s' returned ~s"
                   compile-command compile-val))
          (system (string-append indent-command c-file))
          (system (string-append indent-command header-file))
          (system (string-append "./" base-name))
          (void))))))
