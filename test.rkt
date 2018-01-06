#lang slideshow

(require pict/code)

(require "main-python.rkt")

(require "main-javascript.rkt")
 
(slide
 #:title "Racket+Python+JS"
 (scale (polyglot (define WIDTH 100)
                  (define (my-fact x y)
                    (define x 5)
                    (define (f) 5)
                    (define color (if #t "red" "blue"))
                    (define age (cond ([stephen 4]
                                      [sara 5]
                                      [lindsey 6])))
                    (if (and (= 0 x) (dooby x))
                        1
                        (* x
                           (my-fact
                            (- x 1)))))
                  (string-append "Hello " "World")
                  (map my-fact '(1 2 3 4)))
        1))

(define-syntax polyglot
  (syntax-rules ()
    [(_ expr ...)
     (ht-append 20
                (vl-append 20 (codeblock-pict "#lang racket")
                           (code expr ...))
                (colorize (vline 1 200) "gray")
                (vl-append 20 (codeblock-pict "#lang python")
                           (codeblock-pict
                            (python-str '(begin
                                           expr ...))))
                
                (colorize (vline 1 200) "gray")
                
                (vl-append 20 (codeblock-pict "#lang javascript")
                           (codeblock-pict
                            (javascript-str '(begin
                                               expr ...))))
                )]))
