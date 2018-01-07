#lang racket

(provide javascript-str)

(require "racket2blank.rkt")

(define (javascript-str e)
  (set-TRUE! "true")
  (set-FALSE! "false")

  (set-operator-translations!
   '(
     (remainder . %)
     (expt . **)
     (not . !)
     (eq? . is)
     (and . &&)
     (append . +)
     (list-ref . index)
     (string-append . +)))

  (set-operator-precedence!
   (lambda (op)
     (cdr
      (assoc op
             '(
               ((if) . 1)
               ((||) . 2)
               ((&&) . 3)
               ((!) . 4)
               ((in is < <= > >= == eq?) . 5)

               ((+) . 9.5) ;To deal with 1-(2+3)
               ((+ -) . 10)
               ((*) . 10.5) ;To deal with 1/(2*3)
               ((* / // %) . 11)
            
               ((index call attribute) . 14)
               ((tuple list dict) . 15))
             member))))

  (set-compile-function!
   (lambda (indent return? name params body)
     (advance indent)
     (printf "function ~A(" (my-lang-name name))
     (let ((params
            (if (symbol? params)
                (begin
                  (printf "*~A" (my-lang-name params))
                  (list params))
                (my-lang-parameters params))))
       (printf "){~%")
       (parameterize ((locals (append (locals) params)))
         (my-lang-statements body (+ indent 4) (include-return?))))
     (newline)
     (advance indent)
     (printf "}")))

  (set-compile-set-var!
   (lambda (indent return? name init)
     (advance indent)
     (printf "var ~A = " (my-lang-name name))
     (my-lang-expression init)))


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
   (lambda (indent return? test clause clauses )
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
          (my-lang-statements stmts (+ indent 4) return?))
         (`(,test . ,stmts)
          (printf "}else if(")
          (my-lang-expression test)
          (printf "){~%")
          (my-lang-statements stmts (+ indent 4) return?)
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
                  (display "(")
                  (my-lang-expression test (+ 0.5 op-precedence))
                  (display ")")
                  (display " ? ")
                  (my-lang-expression conseq (+ 0.5 op-precedence))
                  (display " : ")
                  (my-lang-expression altern (+ 0.5 op-precedence)))))


  (set-operators!
   '(** * / // % + - < > <= >= == && ||))

  (set-spacey-operators!
   '(+ - < > <= >= == || &&))

  (my-lang-str e))
