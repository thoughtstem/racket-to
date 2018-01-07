#lang slideshow

(require pict/code)
(require slideshow/play)
(require "racket2processing.rkt")
(require mred) 

(require (for-syntax racket/syntax))
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (timer-renderer c dc)
   (send dc set-scale 3 3)
   (send dc set-text-foreground "black")
   (send dc set-background "white")
   (send dc draw-text (number->string (max 0 (get-field current-seconds c))) 12 8))

(define timer-canvas%
  (class canvas%
    (init-field seconds [current-seconds seconds])
    (define th #f)
    (define/override (on-event event)
      (and (equal? (send event get-event-type) 'left-down)
           (set! current-seconds seconds)))
    
    (define/public (start)
      (and th (kill-thread th))
      (set! th
            (thread
             (λ ()
               (let loop ()
                 (sleep 1)
                 (set! current-seconds (sub1 current-seconds))
                 (send this refresh-now)
                 (loop))))))
    (define/public (stop)
      (kill-thread th))
    (super-new)))

(define (timer t)
  (lambda (p)
    (define c
      (new timer-canvas%
           [seconds t]
           [parent p]
           [paint-callback timer-renderer]))
    (send c start)
    (λ ()
      (send c stop))))

(define (embeddable c)
   (interactive
      (colorize (filled-rectangle 100 100) "white")
      (λ (f)
        (define to-show (c f))
        to-show)))
    







(define (easy-mode-intro-sequence)
  (slide
   #:title "The Game (Easy Mode)"
   (t "We're going to play a game..."))

  (slide
   #:title "The Game (Easy Mode)"
   (t "There will be a series of questions"))

  (slide
   #:title "The Game (Easy Mode)"
   (t "Each question is worth one point"))

  (slide
   #:title "The Game (Easy Mode)"
   (t "I'm not allowed to tell you the answers")
   (t "(But I can give hints)"))

  (slide
   #:title "The Game (Easy Mode)"
   (t "The rules to the game will change over time")
   (t "For now, there are only two rules..."))

  (slide
   #:title "The Game (Easy Mode)"
   (t "RULE 1: To answer a question...")
   (tt "YOU MUST RAISE YOUR HAND"))

  (slide
   #:title "The Game (Easy Mode)"
   (t "RULE 2: Every question has a time limit...")
   (embeddable (timer 10)))

  )

(define (symbol->id id suffix)
  (eval (string->symbol (format "~a-~a" id suffix)) ns))

(define (comparison-slide id #:title (title "What do we notice?") #:footer (footer (blank 0)))
  (slide
   #:title title
   (scale (symbol->id id "side-by-side") 1.2)
   footer))

(define (demo-slide id #:title (title "What do we think this will do?"))
  (slide
   #:title title
   (vc-append 20
              (scale (symbol->id id "uncompiled-img") 1.2))
   (clickback (t "Demo...")
              (thunk (begin
                       (message-box "Loading" "Please wait")
                       (processing (symbol->id id "compiled-str")))))))



(define (qa-slide slide-f id pair)
  (slide-f id #:title (vc-append
                       (t "Question")
                       (colorize
                        (para #:align 'center (first pair))
                        "darkred"))
              #:footer (embeddable (timer (third pair))))
  (slide-f id #:title (vc-append
                       (t "Answer")
                       (colorize
                        (para #:align 'center
                              (second pair))
                        "darkgreen"))))

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
                                                 (colorize (vline 1 200) "gray")
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
   (define (setup)
       (background 255)
       (size 300 300))

   code:blank
   (define (draw)
     (stroke 0 255 0 100)))

(define-example intro-3
   (define (setup)
       (background 255)
       (size 300 300))

   code:blank
   (define (draw)
     (stroke 0 255 0 100)
     (line 0 0 width height)))

(define-example intro-4
   (define (setup)
       (background 255)
       (size 300 300))

   code:blank
   (define (draw)
     (stroke 0 255 0 100)
     (line 0 0 (random 0 width) height)))

(define-example intro-5
   (define (setup)
       (background 255)
       (size 300 300))

   code:blank
   (define (draw)
     (stroke (random 50) (random 255) (random 255) 100)
     (line 0 0 (random 0 width) height)))     




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

(demo-slide 'lines-two #:title "Sneak Peak...")

(easy-mode-intro-sequence)

(socratic-slide-sequence comparison-slide 'intro-1
                         '("What is the language on the left?" "Racket" 10)
                         '("What is the language on the right?" "Java Processing" 10)
                         '("How many functions are in each program?" "1" 30)
                         '("What is the function's name" "setup" 10)
                         '("What will this program do when we run it?" "Let's find out..." 60))

(demo-slide 'intro-1 #:title "")

(socratic-slide-sequence comparison-slide 'intro-1
                         '("In your own words, what did it do?"
                           "Created a blank white background 300 pixels wide and 300 pixels tall"
                           60))

(socratic-slide-sequence comparison-slide 'intro-2
                         '("How many functions are there in each program?" "2" 20)
                         '("What are the function's names" "'setup' and 'draw'" 30)
                         )


(demo-slide 'intro-2)

(comparison-slide 'intro-3)
(demo-slide 'intro-3)

(comparison-slide 'intro-4)
(demo-slide 'intro-4)

(comparison-slide 'intro-5)
(demo-slide 'intro-5)




(comparison-slide 'lines-one)
(demo-slide 'lines-one)

(comparison-slide 'lines-two)
(demo-slide 'lines-two)


;TODO: More activity types

;TODO: Will this also work for 6+ Minetest?







