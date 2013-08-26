#lang racket/base
(require racket/match
         racket/list
         math/base
         2htdp/image
         2htdp/universe)

(define W 800)
(define H 600)
(define FS 36)
(define CHARS
  (shuffle (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(define (gradient start end steps i)
  (+ start (* (- end start) (/ (- steps i) steps))))
(define (radius i n)
  (floor (gradient (/ FS 4) FS n i)))

(struct mouse-task:click (kind x y r))
(define (mouse-task:click* kind r)
  (mouse-task:click
   kind
   (random-integer r (- W r))
   (random-integer r (- H (max r (image-height mouse-legend))))
   r))

(struct mouse-task:drag (from to))
(define (mouse-task:drag* r)
  (mouse-task:drag
   (mouse-task:click* 'from r)
   (mouse-task:click* 'to r)))

(struct state:start (key-n mouse-n))
(struct state:key (time typed to-type k))
(struct state:mouse (time done to-do k))
(struct state:report (key mouse))
(struct state:end ())

(struct result:mouse (task time))
(struct result:key (actual expected time))

(struct report:key (typed))
(struct report:mouse (done))

(define (state:key-start n k)
  (define to-type (take CHARS n))
  (state:key* (current-inexact-milliseconds) empty to-type k))
(define (state:key* t ty to k)
  (if (empty? to)
    (state:mouse-start k (report:key ty))
    (state:key t ty to k)))

(define (state:mouse-start n k)
  (define to-mouse
    (for/list ([i (in-range n)])
      (match (random 4)
        [(or 0 1)
         (mouse-task:click* 'left (radius i n))]
        ;; XXX implement double click and drag
        [2
         (mouse-task:click* 'double (radius i n))]
        [3
         (mouse-task:drag* (radius i n))])

      (mouse-task:click* 'left (radius i n))))
  (state:mouse* (current-inexact-milliseconds) empty to-mouse k))
(define (state:mouse* t do to k)
  (if (empty? to)
    (state:report k (report:mouse do))
    (state:mouse t do to k)))

(define mouse-legend
  (begin
    (beside (text "Once" FS "green")
            (text "Twice" FS "blue")
            (text "     " FS "black")
            (text "Drag" FS "orange")
            (text "To" FS "black"))
    empty-image))

(define state-draw
  (match-lambda
   [(state:start key-n mouse-n)
    (overlay/align
     "center" "middle"
     (text "Press Any Key or Click to Start" FS "black")
     (empty-scene W H))]
   [(state:key _ typed to-type _)
    (define result:key->image
      (match-lambda
       [(result:key a e _)
        (text (string a) (* 2 FS)
              (if (char=? a e)
                "green"
                "red"))]))
    (place-image/align
     (apply beside (list* empty-image empty-image (map result:key->image (reverse typed))))
     (/ W 2) (* H 3/4)
     "right" "middle"
     (place-image/align
      (text (list->string to-type) (* 2 FS) "black")
      (/ W 2) (* H 1/4)
      "left" "middle"
      (empty-scene W H)))]
   [(state:mouse _ _ (cons mt _) _)
    (define (draw-circles mt s)
      (match mt
        [(mouse-task:click kind x y r)
         (place-image
          (circle r
                  (if (eq? 'to kind)
                    "outline"
                    "solid")
                  (match kind
                    ['left "green"]
                    ['double "blue"]
                    ['from "orange"]
                    ['to "black"]))
          x y
          s)]
        [(mouse-task:drag from to)
         (draw-circles from (draw-circles to s))]))
    (draw-circles mt
                  (overlay/align "right" "bottom"
                                 mouse-legend
                                 (empty-scene W H)))]
   [(state:report (report:key ks) (report:mouse ms))
    (define-values (correct-ks mistakes-ks)
      (partition (λ (rk)
                   (char=? (result:key-actual rk) (result:key-expected rk)))
                 ks))
    (define (average l)
      (if (empty? l)
        "n/a"
        (real->decimal-string (/ (sum l) (length l)))))
    (define avg-time-ks
      (average (map result:key-time ks)))
    (define avg-time-ms
      (average (map result:mouse-time ms)))
    (overlay/align
     "left" "top"
     (above/align
      "left"
      (text (format "Typing") FS "black")
      (text (format "            Keys: ~a" (length correct-ks)) FS "black")
      (text (format "        Mistakes: ~a" (length mistakes-ks)) FS "black")
      (text (format "    Average Time: ~ams" avg-time-ks) FS "black")
      (text (format "Mouse") FS "black")
      (text (format "           Tasks: ~a" (length ms)) FS "black")
      (text (format "    Average Time: ~ams" avg-time-ms) FS "black"))
     (empty-scene W H))]
   [(state:end)
    (empty-scene W H)]))

(define (valid-key? ke)
  (and (string? ke)
       (= 1 (string-length ke))
       (member (string-ref ke 0) CHARS)
       (string-ref ke 0)))

(define (state-key s ke)
  (match s
    [(state:start key-n mouse-n)
     (state:key-start key-n mouse-n)]
    [(state:key then typed to-type k)
     (cond
       [(valid-key? ke)
        =>
        (λ (c)
          (define now (current-inexact-milliseconds))
          (define time (- now then))
          (define new-typed
            (cons (result:key c (first to-type) time) typed))
          (if (char=? c (first to-type))
            (state:key* now new-typed (rest to-type) k)
            (state:key* then new-typed to-type k)))]
       [else
        s])]
    [(state:report _ _)
     (state:end)]
    [s
     s]))

(define (distance mx my x y)
  (sqrt (+ (sqr (- mx x)) (sqr (- my y)))))

(define (inside? mx my x y r)
  (<= (distance mx my x y) r))

(define (state-mouse s mx my me)
  (match s
    [(state:start key-n mouse-n)
     (if (mouse=? me "button-down")
       (state:key-start key-n mouse-n)
       s)]
    [(state:mouse then done (cons next more) k)
     (define now (current-inexact-milliseconds))
     (match next
       [(mouse-task:click 'left x y r)
        (if (and (mouse=? me "button-down") (inside? mx my x y r))
          (state:mouse* now (cons (result:mouse next (- now then)) done) more k)
          s)])]
    [s
     s]))

(define (go! key-n mouse-n)
  (big-bang
   (state:start key-n mouse-n)
   [to-draw
    state-draw W H]
   [on-mouse
    state-mouse]
   [on-key
    state-key]
   [stop-when
    state:end?]))


(module+ main
  (go! 25 10))
