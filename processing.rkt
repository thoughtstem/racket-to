#lang racket

(provide processing-str)

(require "racket2blank.rkt")


(define (processing-str e)
  (set-statement-end-token! ";" )
  (set-TRUE! "true")
  (set-FALSE! "false")

  (set-operator-translations!
   '(
     (remainder . %)
     (expt . **)
     (eq? . ==)
     (and . &&)
     (append . +)
     (string-append . +)))

  (set-operator-precedence!
   (lambda (op)
     (cdr
      (assoc op
             '(
               ((if) . 1)
               ((or) . 2)
               ((and) . 3)
               ((not) . 4)
               ((in is < <= > >= == eq?) . 5)

               ((&&) . 3)
            
               ((+) . 9.5) ;To deal with 1-(2+3)
               ((+ -) . 10)
               ((*) . 10.5) ;To deal with 1/(2*3)
               ((* / // %) . 11)
            
               ((index call attribute) . 14)
               ((tuple list dict) . 15))
             member))))

  (set-compile-import!
    (lambda (id)
      (printf (format "import ~a;" id))))
  
  (set-compile-function!
   (lambda (indent return? name params body)
     (define f-type (or (detect-type (symbol->string name) 'void)
                          "void"))
     (advance indent)
     (printf "~a ~A(" f-type
             (my-lang-name name))
     (let ([params
            (if (symbol? params)
                (begin
                  (printf "*~A" (my-lang-name params))
                  (list params))
                (my-lang-parameters params))])
       (printf "){~%")
       (parameterize ((locals (append (locals) params)))
         (my-lang-statements body (+ indent 4) (if (eq? f-type "void")
                                                   #f
                                                   #t))))
     (newline)
     (advance indent)
     (printf "}")))

  (set-compile-set-var!
   (lambda (indent return? name init)
     (advance indent)
     (printf "~a ~A = " (detect-type (symbol->string name) init) (my-lang-name name))
     (my-lang-expression init)))

  (set-unparse-parameter!
      (lambda (param)
        (printf "~a ~a" (detect-type (symbol->string param) 'novalueknown) (my-lang-name param))
        param))

  (define (detect-type name v)
    (or
        (name-detection name)
        (value-detection v)))

  (define (value-detection v)
    (cond  [(and (number? v) (integer? v)) "int"]
           [(number? v) "float"]
           [(and (list? v) (eq? (first v) 'loadImage)) "PImage"]
           [(string? v) "string"]
           [(boolean? v) "boolean"]
           [else #f]))

  (define (name-detection name)
    (define matches (regexp-match #rx"(.*):(.*)" name))
    (if (not matches)
        #f
        (handle-array-type (second matches))))

  (define (handle-array-type t)
     (define matches (regexp-match #rx"(.*)Arr" t))
     (if (not matches)
         t
         (format "~a []" (second matches))))


  (set-compile-if-else-statement!
   (lambda (indent return? test conseq altern)
     (advance indent)
     (printf "if(")
     (my-lang-expression test)
     (printf "){~%")
     (my-lang-statement conseq (+ indent 4) return?)
     (newline)
     (advance indent)
     (printf "}")
     (newline)
     (advance indent)
     (printf "else{~%")
     (my-lang-statement altern (+ indent 4) return?)
     (newline)
     (advance indent)
     (printf "}")))

  (set-compile-if-else-if-statement!
   (lambda (indent return? clause clauses )
     (advance indent)
     (match clause
       (`(,test . ,stmts)
        (printf "if(")
        (my-lang-expression test)
        (printf "){~%")
        (my-lang-statements stmts (+ indent 4) return?)
        (printf "}")
        ))
     (for ((clause clauses))
       (newline)
       (advance indent)
       (match clause
         (`(else . ,stmts)
          (printf "else{~%")
          (my-lang-statements stmts (+ indent 4) return?)
          (newline)
          (advance indent)
          (printf "}"))
         (`(,test . ,stmts)
          (printf "else if(")
          (my-lang-expression test)
          (printf "){~%")
          (my-lang-statements stmts (+ indent 4) return?)
          (newline)
          (advance indent)
          (printf "}")
          )
         (`...
          (printf "..."))))))



  (set-compile-begin!
   (lambda (indent return? stmt stmts)
     (my-lang-statement stmt indent)
     (newline)
     (my-lang-statements stmts indent return?)))


  (set-additional-matchers!
   (lambda (e rec)
     (match e
       (`(error ',name ,str . ,args)
        (rec `(error (format ,(string-append "~A: " str) ,(my-lang-name name) . ,args)))
        #t)
       (`(foldl ,f ,i ,l)
        (rec `(reduce ,f ,l ,i))
        #t)
       (`(map ,f ,l)
        (my-lang-expression l)
        (display ".map(")
        (my-lang-expression f)
        (display ")")
        #t)
       (`(string-join ,strs ,el)
        (rec `(attribute ,el (join ,strs)))
        #t)
       (else #f))))

  (set-compile-inline-if!
   (lambda (e rec precedence test conseq altern)
     (maybe-paren ('if op-precedence precedence)
                  (my-lang-expression test (+ 0.5 op-precedence))
                  (display " ? ")
                  (my-lang-expression conseq (+ 0.5 op-precedence))
                  (display " : ")
                  (my-lang-expression altern (+ 0.5 op-precedence)))))


  (set-operators!
   '(or and is ** * / // % + - < > <= >= == &&))

  (set-spacey-operators!
   '(+ - < > <= >= == or and &&))

  (my-lang-str e))
