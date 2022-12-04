#lang racket/base

;; https://github.com/msgpack/msgpack/blob/8aa09e2a6a9180a49fc62ecfefe149f063cc5e4b/spec.md

(require racket/fixnum
         "nil.rkt")

(provide
 current-msgpack-ext-read-handler
 read-msgpack)

(define current-msgpack-ext-read-handler
  (make-parameter #f))

(define (read-msgpack in)
  (read-msgpack* (current-msgpack-ext-read-handler) in))

(define (read-msgpack* ext in)
  (define b
    (read-byte in))
  (cond
    [(eof-object? b)
     (error 'read-msgpack "unexpected EOF")]
    [(and (fx>= b #x00) (fx<= b #x7F)) b]
    [(and (fx>= b #x80) (fx<= b #x8F)) (read-map ext (fxand b #b1111) in)]
    [(and (fx>= b #x90) (fx<= b #x9F)) (read-array ext (fxand b #b1111) in)]
    [(and (fx>= b #xA0) (fx<= b #xBF)) (read-msgpack-string (fxand b #b11111) in)]
    [(and (fx>= b #xE0) (fx<= b #xFF)) (- (fxand b #b11111) 32)]
    [else
     (case b
       [(#xC0) msgpack-nil]
       [(#xC2) #f]
       [(#xC3) #t]
       [(#xC4) (read-msgpack-bytes (read-length 1 in) in)]
       [(#xC5) (read-msgpack-bytes (read-length 2 in) in)]
       [(#xC6) (read-msgpack-bytes (read-length 4 in) in)]
       [(#xC7) (read-ext ext (read-length 1 in) in)]
       [(#xC8) (read-ext ext (read-length 2 in) in)]
       [(#xC9) (read-ext ext (read-length 4 in) in)]
       [(#xCA) (read-float 4 in)]
       [(#xCB) (read-float 8 in)]
       [(#xCC) (read-integer 1 #f in)]
       [(#xCD) (read-integer 2 #f in)]
       [(#xCE) (read-integer 4 #f in)]
       [(#xCF) (read-integer 8 #f in)]
       [(#xD0) (read-integer 1 #t in)]
       [(#xD1) (read-integer 2 #t in)]
       [(#xD2) (read-integer 4 #t in)]
       [(#xD3) (read-integer 8 #t in)]
       [(#xD4) (read-ext ext 1 in)]
       [(#xD5) (read-ext ext 2 in)]
       [(#xD6) (read-ext ext 4 in)]
       [(#xD7) (read-ext ext 8 in)]
       [(#xD8) (read-ext ext 16 in)]
       [(#xD9) (read-msgpack-string (read-length 1 in) in)]
       [(#xDA) (read-msgpack-string (read-length 2 in) in)]
       [(#xDB) (read-msgpack-string (read-length 4 in) in)]
       [(#xDC) (read-array ext (read-length 2 in) in)]
       [(#xDD) (read-array ext (read-length 4 in) in)]
       [(#xDE) (read-map ext (read-length 2 in) in)]
       [(#xDF) (read-map ext (read-length 4 in) in)]
       [else (error 'read-msgpack "unexpected control byte: ~s" b)])]))

(define (read-integer len signed? in)
  (integer-bytes->integer
   (expect-bytes 'read-integer len in)
   signed? #t))

(define (read-float len in)
  (floating-point-bytes->real (expect-bytes 'read-float len in) #t))

(define (read-length len-bytes in)
  (integer-bytes->integer
   (expect-bytes 'read-length len-bytes in) #f #t))

(define (read-msgpack-string len in)
  (bytes->string/utf-8
   (expect-bytes 'read-msgpack-string len in)))

(define (read-msgpack-bytes len in)
  (expect-bytes 'read-msgpack-bytes len in))

(define (read-array ext len in)
  (for/list ([_ (in-range len)])
    (read-msgpack* ext in)))

(define (read-map ext len in)
  (for/hash ([_ (in-range len)])
    (values
     (read-msgpack* ext in)
     (read-msgpack* ext in))))

(define (read-ext proc len in)
  (define ext-tag
    (read-byte in))
  (define ext-bs
    (expect-bytes 'read-ext len in))
  (cond
    [(> ext-tag #x7F)
     (case ext-tag
       [(#xFF)
        (parse-ext-timestamp ext-bs)]
       [else
        (error 'read-ext "reserved extension tag: ~s" ext-tag)])]
    [proc
     (proc ext-tag ext-bs)]
    [else
     (error 'read-ext "no extension handler for tag: ~s" ext-tag)]))

(define (parse-ext-timestamp ext-bs)
  (define-values (seconds nanos)
    (case (bytes-length ext-bs)
      [(4)
       (values (integer-bytes->integer ext-bs #f #t) 0)]
      [(8)
       (define timestamp
         (integer-bytes->integer ext-bs #f #t))
       (values
        (bitwise-and timestamp #x7FFFFFFFF)
        (arithmetic-shift timestamp -34))]
      [(12)
       (values
        (integer-bytes->integer ext-bs #f #t 4 12)
        (integer-bytes->integer ext-bs #f #t 0 4))]
      [else
       (error 'read-ext-timestamp "unexpected timestamp data: ~.s" ext-bs)]))
  (struct-copy date*
               (seconds->date seconds #f)
               [nanosecond nanos]))

(define (expect-bytes who n in)
  (define bs (read-bytes n in))
  (begin0 bs
    (when (or (eof-object? bs)
              (< (bytes-length bs) n))
      (error who "unexpected EOF"))))
