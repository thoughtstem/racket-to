#lang racket

(provide javascript javascript-str javascript-statement javascript-expression)


(define operator-translations
  '(
    (remainder . %)
    (expt . **)
    (eq? . is)
    (and . &&)
    (append . +)
    (list-ref . index)
    (string-append . +)))


(define (operator-precedence op)
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
          member)))

(define (javascript-name name)
  (let ((str (symbol->string name)))
    (string-replace
     str
     "-"
     "_")))

(define (advance n)
  (for ((i n))
    (display " ")))

(define (unparse-parameter param)
  (display (javascript-name param))
  param)

(define (javascript-parameters params)
  (cond ((null? params)
         (list))
        (else
         (cons (unparse-parameter (car params))
               (for/list ((param (cdr params)))
                 (display ", ")
                 (unparse-parameter param))))))

(define locals (make-parameter (list)))

(define include-return? (make-parameter #t))

(define (compile-function indent return? name params body)
  (advance indent)
  (printf "function ~A(" (javascript-name name))
  (let ((params
         (if (symbol? params)
             (begin
               (printf "*~A" (javascript-name params))
               (list params))
             (javascript-parameters params))))
    (printf "){~%")
    (parameterize ((locals (append (locals) params)))
      (javascript-statements body (+ indent 4) (include-return?))))
  (newline)
  (advance indent)
  (printf "}"))

(define (compile-set-var indent return? name init)
  (advance indent)
  (printf "~A = " (javascript-name name))
  (javascript-expression init))


(define (compile-if-else-statement indent return? test conseq altern)
  (advance indent)
     (printf "if(")
     (javascript-expression test)
     (printf "){~%")
     (javascript-statement conseq (+ indent 4) return?)
     (newline)
     (advance indent)
     (printf "}")
     (newline)
     (advance indent)
     (printf "else{~%")
     (javascript-statement altern (+ indent 4) return?)
     (newline)
     (advance indent)
     (printf "}"))

(define (compile-if-else-if-statement indent return? test clause clauses )
  (advance indent)
     (match clause
       (`(,test . ,stmts)
        (printf "if(")
        (javascript-expression test)
        (printf "){~%")
        (javascript-statements stmts (+ indent 4) return?)
        (printf "}")
        ))
     (for ((clause clauses))
       (newline)
       (advance indent)
       (match clause
         (`(else . ,stmts)
          (printf "else{~%")
          (javascript-statements stmts (+ indent 4) return?))
         (`(,test . ,stmts)
          (printf "}else if(")
          (javascript-expression test)
          (printf "){~%")
          (javascript-statements stmts (+ indent 4) return?)
          (printf "}")
          )
         (`...
          (printf "...")))))

(define (compile-begin indent return? stmt stmts)
  (javascript-statement stmt indent)
  (newline)
  (javascript-statements stmts indent return?))

(define (javascript-statement stmt [indent 0] [return? #f])
  (match stmt
    ((or `(define (,name . ,params) : ,_ . ,body)
         `(define (,name . ,params) . ,body))
     (compile-function indent return? name params body))
    ((or `(define ,name ,init)
         `(define ,name : ,_ ,init))
     (compile-set-var indent return? name init))
    (`(if ,test ,conseq ,altern)
     (compile-if-else-statement indent return? test conseq altern))
    (`(cond ,clause . ,clauses)
     (compile-if-else-if-statement indent return? clause clauses))
    (`(begin ,stmt . ,stmts)
     (compile-begin indent return? stmt stmts))
    (`(error . ,_)
     (javascript-maybe-indent-expression "raise " stmt indent))
    (else
     (javascript-maybe-indent-expression (if return? "return " "") stmt indent))))


(define (javascript-statements stmts indent [return? #f])
  (let ((first? #t))
        (for ((stmt (drop-right stmts 1)))
          (cond (first?
                    (javascript-statement stmt indent)
                    (set! first? #f))
                   (else
                    (newline)
                    (javascript-statement stmt indent))))
        (unless first? (newline))
        (javascript-statement (last stmts) indent return?)))



(define-syntax-rule
  (maybe-paren (op op-precedence precedence) e ...)
  (let ((op-precedence (operator-precedence op)))
    (let ((paren? (> precedence op-precedence)))
      (when paren? (display "("))
      e ...
      (when paren? (display ")")))))




;;To take into account the need for indentation in function calls
(define javascript-line-limit 80)
(define break-line? (make-parameter #f))

(define (javascript-maybe-indent-expression start expr indent)
  (let ((so (open-output-string)))
    (port-count-lines! so)
    (parameterize ((current-output-port so)
                   (break-line? #f))
      (advance indent)
      (printf start)
      (javascript-expression expr))
    (let ((str (get-output-string so)))
      (if (> (+ (string-length str) indent) javascript-line-limit)
          (let ((so (open-output-string)))
            (port-count-lines! so)
            (parameterize ((current-output-port so)
                           (break-line? #t))
              (advance indent)
              (printf start)
              (javascript-expression expr))
            (display (get-output-string so)))
          (display str)))))

(define (javascript-call fn args)
  (javascript-expression fn (operator-precedence 'call))
  (display "(")
  (cond ((null? args)
         )
        ((break-line?)
         (let-values (((line column pos) (port-next-location (current-output-port))))
           (parameterize ((break-line? #f)) (javascript-expression (first args)))
           (for ((arg (rest args)))
             (display ",")
             (newline)
             (advance column)
             (parameterize ((break-line? #f)) (javascript-expression arg)))))
        (else
         (javascript-expressions args)))
  (display ")"))




(define (additional-matchers e rec)
  (match e
    (`(error ',name ,str . ,args)
     (rec `(error (format ,(string-append "~A: " str) ,(javascript-name name) . ,args)))
     #t)
    (`(foldl ,f ,i ,l)
     (rec `(reduce ,f ,l ,i))
     #t)
    (`(map ,f ,l)
     (javascript-expression l)
     (display ".map(")
     (javascript-expression f)
     (display ")")
     #t)
    (`(string-join ,strs ,el)
     (rec `(attribute ,el (join ,strs)))
     #t)
    (else #f)))

(define (compile-inline-if e rec precedence test conseq altern)
  (maybe-paren ('if op-precedence precedence)
                   (javascript-expression conseq (+ 0.5 op-precedence))
                   (display " if ")
                   (javascript-expression test (+ 0.5 op-precedence))
                   (display " else ")
                   (javascript-expression altern (+ 0.5 op-precedence))))


(define operators
  '(or and is ** * / // % + - < > <= >= == &&))

(define spacey-operators
  '(is + - < > <= >= == or and &&))


(define (javascript-expression e [precedence 0])
  (define (rec e) (javascript-expression e precedence))
  (or
   (additional-matchers e rec)
   (match e
     (`(if ,test ,conseq ,altern)
      (compile-inline-if e rec precedence test conseq altern))
     
     (`(,(? (lambda (op) (dict-has-key? operator-translations op)) op) . ,args)
      (rec `(,(dict-ref operator-translations op) . ,args)))
     
     (`(,(? (lambda (op)
              (member op operators))
            op)
        ,arg . ,args)
      (maybe-paren (op op-precedence precedence)
                   (let ((space? (member op spacey-operators)))
                     (javascript-expression
                      arg
                      (if (eq? op '**) ;associate from right to left
                          (+ op-precedence 0.5)
                          op-precedence))
                     (for ((arg args))
                       (when space? (display " "))
                       (display op) 
                       (when space? (display " "))
                       (javascript-expression arg op-precedence)))))


     (`(list . ,args)
      (display "[")
      (javascript-expressions args)
      (display "]"))
     
     (`'()
      (rec `(list)))
     (`'(,first . ,rest)
      (rec `(list ,first . ,rest)))
 
    
     ((or `(= . ,args)
          `(=c? . ,args))
      (rec `(== . ,args)))
     (`(,(and (or `not `- `+) op) ,arg)
      (let ((unary-op (dict-ref '((+ . unary+) (- . unary-) (~ . unary~)) op op)))
        (maybe-paren (unary-op op-precedence precedence)
                     (let ((space? (member op '(not))))
                       (display op)
                       (when space? (display " "))
                       (javascript-expression arg op-precedence)))))


     (`(apply ,fn . ,args)
      (rec
          `(,fn ,@(drop-right args 1) (splice-arg ,(last args)))))
     (`(,fn . ,args)
      (javascript-call fn args)) 
     ((? number? x)
      (cond ((inexact? x)
             (display x))
            ((and (rational? x) (not (integer? x)))
             (rec `(/ ,(numerator x) ,(denominator x))))
            (else
             (display x))))
     ((? string? x)
      (if (string-contains? x "\'")
          (print x)
          (begin
            (display "'")
            (display x)
            (display "'"))))
     ((or #t `\#t) ;;to also handle latex
      (if (include-return?)
          (display "True")
          (display "pass")))
     ((or #f `\#f)
      (display "False")) 
     ((? symbol? x)
      (display (javascript-name x)))

     )))


(define (javascript-expressions args)
  (cond ((null? args))
        (else
         (javascript-expression (car args))
         (for ((arg (cdr args)))
           (display ", ")
           (javascript-expression arg)))))

(define (javascript e [indent 0])
  (match e
    (`(define . ,body)
     (javascript-statement `(define . ,body)))
    (else
     (javascript-statement e indent))))

(define (javascript-str rkt #:return? [return? #t])
  (let ((so (open-output-string)))
    (parameterize ((current-output-port so)
                   (include-return? return?))
      (javascript rkt))
    (get-output-string so)))
