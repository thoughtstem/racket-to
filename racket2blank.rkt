#lang racket


(provide (all-defined-out))

(require (for-syntax racket/syntax))

(define-syntax (compile-hook stx)
  (syntax-case stx ()
    [(_ id )
     (with-syntax* ([setter (format-id stx "set-~a!" #'id)])
       #`(begin
           (provide id)
           (define id #f)
           (define (setter v)
             (set! id v))))]))

(compile-hook compile-if-else-statement)
(compile-hook compile-if-else-if-statement)
(compile-hook compile-begin)
(compile-hook compile-set-var)
(compile-hook compile-function)
(compile-hook operator-precedence)
(compile-hook additional-matchers)
(compile-hook FALSE)
(compile-hook TRUE)
(compile-hook operators)
(compile-hook spacey-operators)
(compile-hook operator-translations)
(compile-hook compile-inline-if)


(define (my-lang-name name)
  (let ((str (symbol->string name)))
    (string-replace
     str
     "-"
     "_")))

(define (advance n)
  (for ((i n))
    (display " ")))

       

(define (unparse-parameter param)
  (display (my-lang-name param))
  param)

(define (my-lang-parameters params)
  (cond ((null? params)
         (list))
        (else
         (cons (unparse-parameter (car params))
               (for/list ((param (cdr params)))
                 (display ", ")
                 (unparse-parameter param))))))

(define locals (make-parameter (list)))

(define include-return? (make-parameter #t))



(define (my-lang-statement stmt [indent 0] [return? #f])
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
     (my-lang-maybe-indent-expression "raise " stmt indent))
    (else
     (my-lang-maybe-indent-expression (if return? "return " "") stmt indent))))


(define (my-lang-statements stmts indent [return? #f])
  (let ((first? #t))
    (for ((stmt (drop-right stmts 1)))
      (cond (first?
             (my-lang-statement stmt indent)
             (set! first? #f))
            (else
             (newline)
             (my-lang-statement stmt indent))))
    (unless first? (newline))
    (my-lang-statement (last stmts) indent return?)))




;;To take into account the need for indentation in function calls
(define my-lang-line-limit 80)
(define break-line? (make-parameter #f))

(define (my-lang-maybe-indent-expression start expr indent)
  (let ((so (open-output-string)))
    (port-count-lines! so)
    (parameterize ((current-output-port so)
                   (break-line? #f))
      (advance indent)
      (printf start)
      (my-lang-expression expr))
    (let ((str (get-output-string so)))
      (if (> (+ (string-length str) indent) my-lang-line-limit)
          (let ((so (open-output-string)))
            (port-count-lines! so)
            (parameterize ((current-output-port so)
                           (break-line? #t))
              (advance indent)
              (printf start)
              (my-lang-expression expr))
            (display (get-output-string so)))
          (display str)))))


(define (my-lang-call fn args)
  (my-lang-expression fn (operator-precedence 'call))
  (display "(")
  (cond ((null? args)
         )
        ((break-line?)
         (let-values (((line column pos) (port-next-location (current-output-port))))
           (parameterize ((break-line? #f)) (my-lang-expression (first args)))
           (for ((arg (rest args)))
             (display ",")
             (newline)
             (advance column)
             (parameterize ((break-line? #f)) (my-lang-expression arg)))))
        (else
         (my-lang-expressions args)))
  (display ")"))



(define (my-lang-expression e [precedence 0])
  (define (rec e) (my-lang-expression e precedence))
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
                     (my-lang-expression
                      arg
                      op-precedence)
                     (for ((arg args))
                       (when space? (display " "))
                       (display op) 
                       (when space? (display " "))
                       (my-lang-expression arg op-precedence)))))
     
     (`(list . ,args)
      (display "[")
      (my-lang-expressions args)
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
                       (my-lang-expression arg op-precedence)))))


     (`(apply ,fn . ,args)
      (rec
          `(,fn ,@(drop-right args 1) (splice-arg ,(last args)))))
     (`(,fn . ,args)
      (my-lang-call fn args)) 
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
          (display TRUE)
          (display "pass")))
     ((or #f `\#f)
      (display FALSE)) 
     ((? symbol? x)
      (display (my-lang-name x)))

     )))


(define (my-lang-expressions args)
  (cond ((null? args))
        (else
         (my-lang-expression (car args))
         (for ((arg (cdr args)))
           (display ", ")
           (my-lang-expression arg)))))

(define (my-lang e [indent 0])
  (match e
    (`(define . ,body)
     (my-lang-statement `(define . ,body)))
    (else
     (my-lang-statement e indent))))

(define (my-lang-str rkt #:return? [return? #t])
  (let ((so (open-output-string)))
    (parameterize ((current-output-port so)
                   (include-return? return?))
      (my-lang rkt))
    (get-output-string so)))


      

       


(define-syntax-rule
  (maybe-paren (op op-precedence precedence) e ...)
  (let ((op-precedence (operator-precedence op)))
    (let ((paren? (> precedence op-precedence)))
      (when paren? (display "("))
      e ...
      (when paren? (display ")")))))
