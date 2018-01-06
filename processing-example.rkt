#lang slideshow

(require pict/code)
(require slideshow/play)
(require "racket2processing.rkt")

(require (for-syntax racket/syntax))
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (symbol->id id suffix)
  (eval (string->symbol (format "~a-~a" id suffix)) ns))

(define (comparison-slide id #:title (title "What do we notice?"))
  (slide
   #:title title
   (scale (symbol->id id "side-by-side") 1.2)))

(define (demo-slide id #:title (title "What do we think this will do?"))
  (slide
   #:title title
   (vc-append 20
              (scale (symbol->id id "uncompiled-img") 1.2))
   (clickback (t "Demo...")
              (thunk (processing (symbol->id id "compiled-str"))))))

(define (qa-slide slide-f id pair)
  (slide-f id #:title (vc-append
                       (t "Question")
                       (colorize
                        (para #:align 'center
                              (first pair))
                        "darkgreen")))
  (slide-f id #:title (vc-append
                       (t "Answer")
                       (colorize
                        (para #:align 'center
                              (second pair))
                        "darkred"))))

(define (socratic-slide-sequence slide-f id . qas)
  (map (curry qa-slide slide-f id) qas))


(define-syntax (define-example stx)
  (syntax-case stx ()
    [(_ id expr ... )
     (with-syntax* ([data-id (format-id stx "~a" #'id)]
                    [uncompiled-id (format-id stx "~a-uncompiled-img" #'id)]
                    [compiled-id (format-id stx "~a-compiled-img" #'id)]
                    [compiled-str-id (format-id stx "~a-compiled-str" #'id)]
                    [side-by-side-id (format-id stx "~a-side-by-side" #'id)])
       #`(begin
           (define data-id '(expr ...))
           (define compiled-str-id (processing-str '(begin
                                                      expr ...)))
           (define uncompiled-id (vl-append 20 (codeblock-pict "#lang racket")
                                               (code expr ...)))
           (define compiled-id (vl-append 20 (codeblock-pict "#lang java-processing")
                                             (codeblock-pict compiled-str-id)))
           (define side-by-side-id (ht-append 20 uncompiled-id
                                                compiled-id))
           ))]))





(define (processing str)
  (make-directory* "demo")
  (with-output-to-file "demo/demo.pde" #:exists 'replace
      (lambda () (printf str)))
  (displayln "Running processing... Please wait...")
  (system "./processing/processing-java --sketch=demo --run demo/demo.pde"))




;Build up....

(define-example intro-1
  (define (setup)
       (background 255)
       (size 300 300)))

(define-example intro-2
   (define i 0)

   code:blank
   (define (setup)
       (background 255)
       (size 300 300))

   code:blank
   (define (draw)
     (stroke 0 255 0 100)))

(define-example intro-3
   (define i 0)

   code:blank
   (define (setup)
       (background 255)
       (size 300 300))

   code:blank
   (define (draw)
     (stroke (random 50) (random 255) (random 255) 100)))


;Edit...

(define-example lines-one
   (define i 0)

   code:blank
   (define (setup)
       (background 255)
       (size 300 300))

   code:blank
   (define (draw)
     (stroke (random 50)
             (random 255)
             (random 255)
             100)
     (line i 0 (random 0 width) height)
     (if (< i width)
         (set! i (+ 1 i))
         (set! i 0))))


(define-example lines-two
   (define i 0)

   code:blank
   (define (setup)
       (background 0)
       (size 300 300))

   code:blank
   (define (draw)
     (stroke (random 255)
             (random 0)
             (random 0)
             100)
     (line i 0 (random 0 width) height)
     (if (< i width)
         (set! i (+ 1 i))
         (set! i 0))))


(socratic-slide-sequence comparison-slide 'intro-1
                         '("What is the language on the left?" "Racket")
                         '("What is the language on the right?" "Java Processing")
                         '("How many functions are in each program?" "1")
                         '("What is the function's name" "setup")
                         '("What will this program do when we run it?" "Let's find out..."))

(demo-slide 'intro-1 #:title "")

(socratic-slide-sequence comparison-slide 'intro-1
                         '("In your own words, what did it do?"
                           "Created a blank white background 300 pixels wide and 300 pixels tall"))

(socratic-slide-sequence comparison-slide 'intro-2
                         '("How many functions are there in each program?" "2")
                         '("What is the function's name" "'setup' and 'draw'")
                         )


(demo-slide 'intro-2)

(comparison-slide 'intro-3)
(demo-slide 'intro-3)


(comparison-slide 'lines-one)
(demo-slide 'lines-one)

(comparison-slide 'lines-two)
(demo-slide 'lines-two)


;TODO: More activity types

;TODO: Will this also work for 6+ Minetest?







