#lang racket/base

;; https://github.com/msgpack/msgpack/blob/8aa09e2a6a9180a49fc62ecfefe149f063cc5e4b/spec.md

(require racket/date
         racket/fixnum
         "nil.rkt")

(provide
 current-msgpack-ext-write-handler
 write-msgpack)

(define current-msgpack-ext-write-handler
  (make-parameter #f))

(define write-msgpack
  (case-lambda
    [(v)
     (write-msgpack v (current-output-port))]
    [(v out)
     (write-msgpack v (make-bytes 8) out)]
    [(v buf out)
     (define ext (current-msgpack-ext-write-handler))
     (write-msgpack* v ext buf out)]))

(define (write-msgpack* v ext buf out)
  (cond
    [(msgpack-nil? v)
     (write-nil out)]
    [(boolean? v)
     (write-bool v out)]
    [(exact-integer? v)
     (write-integer v buf out)]
    [(real? v)
     (write-float v buf out)]
    [(string? v)
     (write-msgpack-string v buf out)]
    [(bytes? v)
     (write-msgpack-bytes v buf out)]
    [(list? v)
     (write-array v ext buf out)]
    [(hash? v)
     (write-map v ext buf out)]
    [(date*? v)
     (write-ext v ext-timestamp-proc buf out)]
    [ext
     (write-ext v ext buf out)]
    [else
     (raise-argument-error 'write-msgpack "msgpack-value/c" v)])
  (void))

(define (write-nil out)
  (write-byte #xC0 out))

(define (write-bool v out)
  (write-byte (if v #xC3 #xC2) out))

(define (write-integer v buf out)
  (cond
    [(>= v 0)
     (cond
       [(<= v #b01111111)
        (write-byte v out)]
       [(<= v #xFF)
        (write-byte #xCC out)
        (write-byte v out)]
       [(<= v #xFFFF)
        (write-byte #xCD out)
        (integer->integer-bytes v 2 #f #t buf)
        (write-bytes buf out 0 2)]
       [(<= v #xFFFFFFFF)
        (write-byte #xCE out)
        (integer->integer-bytes v 4 #f #t buf)
        (write-bytes buf out 0 4)]
       [(<= v #xFFFFFFFFFFFFFFFF)
        (write-byte #xCF out)
        (integer->integer-bytes v 8 #f #t buf)
        (write-bytes buf out 0 8)]
       [else
        (raise-argument-error 'write-integer "(integer-in #x-8000000000000000 #xFFFFFFFFFFFFFFFF)" v)])]
    [else
     (cond
       [(>= v #b-00011111)
        (write-byte (fxand #xFF (fxior v #b11100000)) out)]
       [(>= v #x-80)
        (write-byte #xD0 out)
        (integer->integer-bytes v 1 #t #t buf)
        (write-bytes buf out 0 1)]
       [(>= v #x-8000)
        (write-byte #xD1 out)
        (integer->integer-bytes v 2 #t #t buf)
        (write-bytes buf out 0 2)]
       [(>= v #x-80000000)
        (write-byte #xD2 out)
        (integer->integer-bytes v 4 #t #t buf)
        (write-bytes buf out 0 4)]
       [(>= v #x-8000000000000000)
        (write-byte #xD3 out)
        (integer->integer-bytes v 8 #t #t buf)
        (write-bytes buf out 0 8)]
       [else
        (raise-argument-error 'write-integer "(integer-in #x-8000000000000000 #xFFFFFFFFFFFFFFFF)" v)])]))

(define (write-float v buf out)
  (real->floating-point-bytes v 8 #t buf)
  (write-byte #xCB out)
  (write-bytes buf out 0 8))

(define (write-msgpack-string v buf out)
  (define bs
    (string->bytes/utf-8 v))
  (define len
    (bytes-length bs))
  (cond
    [(<= len #b00011111)
     (write-byte (fxior #b10100000 len) out)]
    [(<= len #xFF)
     (write-byte #xD9 out)
     (write-byte len out)]
    [(<= len #xFFFF)
     (write-byte #xDA out)
     (integer->integer-bytes len 2 #f #t buf)
     (write-bytes buf out 0 2)]
    [(<= len #xFFFFFFFF)
     (write-byte #xDB out)
     (integer->integer-bytes len 4 #f #t buf)
     (write-bytes buf out 0 4)]
    [else
     (raise-argument-error 'write-msgpack-string "(bytes-length<=/c #xFFFFFFFF)" v)])
  (write-bytes bs out))

(define (write-msgpack-bytes v buf out)
  (define len
    (bytes-length v))
  (cond
    [(<= len #xFF)
     (write-byte #xC4 out)
     (write-byte len out)]
    [(<= len #xFFFF)
     (write-byte #xC5 out)
     (integer->integer-bytes len 2 #f #t buf)
     (write-bytes buf out 0 2)]
    [(<= len #xFFFFFFFF)
     (write-byte #xC6 out)
     (integer->integer-bytes len 4 #f #t buf)
     (write-bytes buf out 0 4)]
    [else
     (raise-argument-error 'write-msgpack-string "(bytes-length<=/c #xFFFFFFFF)" v)])
  (write-bytes v out))

(define (write-array v ext buf out)
  (define len
    (length v))
  (cond
    [(<= len #b00001111)
     (write-byte (fxior #b10010000 len) out)]
    [(<= len #xFFFF)
     (write-byte #xDC out)
     (integer->integer-bytes len 2 #f #t buf)
     (write-bytes buf out 0 2)]
    [(<= len #xFFFFFFFF)
     (write-byte #xDD out)
     (integer->integer-bytes len 4 #f #t buf)
     (write-bytes buf out 0 4)]
    [else
     (raise-argument-error 'write-array "(length<=/c #xFFFFFFFF)" v)])
  (for ([v (in-list v)])
    (write-msgpack* v ext buf out)))

(define (write-map v ext buf out)
  (define len
    (hash-count v))
  (cond
    [(<= len #b00001111)
     (write-byte (fxior #b10000000 len) out)]
    [(<= len #xFFFF)
     (write-byte #xDE out)
     (integer->integer-bytes len 2 #f #t buf)
     (write-bytes buf out 0 2)]
    [(<= len #xFFFFFFFF)
     (write-byte #xDF out)
     (integer->integer-bytes len 4 #f #t buf)
     (write-bytes buf out 0 4)]
    [else
     (raise-argument-error 'write-map "(hash-count<=/c #xFFFFFFFF)" v)])
  (for ([(k v) (in-hash v)])
    (write-msgpack* k ext buf out)
    (write-msgpack* v ext buf out)))

(define (write-ext v proc buf out)
  (define-values (ext-tag ext-bs)
    (proc v))
  (define len
    (bytes-length ext-bs))
  (case len
    [(1 2 4 8 16)
     (write-byte
      (case len
        [(1)  #xD4]
        [(2)  #xD5]
        [(4)  #xD6]
        [(8)  #xD7]
        [(16) #xD8])
      out)
     (write-byte ext-tag out)
     (write-bytes ext-bs out)]
    [else
     (cond
       [(<= len #xFF)
        (write-byte #xC7 out)
        (write-byte len out)]
       [(<= len #xFFFF)
        (write-byte #xC8 out)
        (integer->integer-bytes len 2 #f #t buf)
        (write-bytes buf out 0 2)]
       [(<= len #xFFFFFFFF)
        (write-byte #xC9 out)
        (integer->integer-bytes len 4 #f #t buf)
        (write-bytes buf out 0 4)]
       [else
        (raise-argument-error 'write-ext "(bytes-length<=/c #xFFFFFFFF)" ext-bs)])
     (write-byte ext-tag out)
     (write-bytes ext-bs out)]))

(define (ext-timestamp-proc v)
  (define seconds (date->seconds v #f))
  (define nanos (date*-nanosecond v))
  (define bs (make-bytes 12))
  (begin0 (values #xFF bs)
    (integer->integer-bytes nanos 4 #f #t bs)
    (integer->integer-bytes seconds 8 #f #t bs 4)))
