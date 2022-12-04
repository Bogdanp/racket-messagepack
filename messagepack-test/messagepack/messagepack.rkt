#lang racket/base

(require messagepack
         rackcheck
         racket/date
         racket/list
         racket/port
         rackunit)

(define gen:messagepack-scalar
  (gen:choice
   (gen:const msgpack-nil)
   (gen:const #t)
   (gen:const #f)
   gen:natural
   gen:real
   (gen:bytes #:max-length (* 10 1024 1024))
   (gen:string #:max-length (* 10 1024 1024))))

(define gen:messagepack*
  (gen:delay
   (gen:frequency
    `((1000 . ,gen:messagepack-scalar)
      (   1 . ,gen:messagepack)))))

(define gen:messagepack
  (gen:choice
   gen:messagepack-scalar
   (gen:list gen:messagepack*)
   (gen:let ([n (gen:integer-in 0 32)]
             [ks (apply gen:tuple (make-list n gen:messagepack*))]
             [vs (apply gen:tuple (make-list n gen:messagepack*))])
     (for/hash ([k (in-list ks)]
                [v (in-list vs)])
       (values k v)))))

(define buf (open-output-bytes))
(define (roundtrip v)
  (write-msgpack v buf)
  (call-with-input-bytes (get-output-bytes buf #t)
    read-msgpack))

(define-property prop:roundtrip
  ([v gen:messagepack])
  (check-equal? v (roundtrip v)))

(define-check (check-roundtrip v)
  (define read-v
    (roundtrip v))
  (with-check-info
    (['value read-v]
     ['expected v])
    (unless (equal? v read-v)
      (fail-check))))

(define messagepack-suite
  (test-suite
   "messagepack"

   (test-suite
    "basics"

    (test-suite
     "nil"

     (check-roundtrip msgpack-nil))

    (test-suite
     "bool"

     (check-roundtrip #t)
     (check-roundtrip #f))

    (test-suite
     "integer"

     (check-roundtrip 0)
     (check-roundtrip #x7F)
     (check-roundtrip #x80)
     (check-roundtrip #x7FFF)
     (check-roundtrip #x8000)
     (check-roundtrip #x7FFFFFFF)
     (check-roundtrip #xFFFFFFFFFFFFFFFF)
     (check-roundtrip #x8000)
     (check-roundtrip #x-8000)
     (check-roundtrip #x80000000)
     (check-roundtrip #x-80000000)
     (check-roundtrip #x8000000000000000)
     (check-roundtrip #x-8000000000000000)
     (check-roundtrip -1)
     (check-roundtrip -16)
     (check-roundtrip -31)
     (check-roundtrip -32))

    (test-suite
     "float"

     (check-roundtrip 0.0)
     (check-roundtrip +inf.0)
     (check-roundtrip -0.0)
     (check-roundtrip -5.0)
     (check-roundtrip 15.0))

    (test-suite
     "string"

     (check-roundtrip "")
     (check-roundtrip "hello, world"))

    (test-suite
     "bytes"

     (check-roundtrip #"")
     (check-roundtrip #"hello, world"))

    (test-suite
     "array"

     (check-roundtrip '())
     (check-roundtrip '(#t #f "hello" 1 2 3.0)))

    (test-suite
     "map"

     (check-roundtrip (hash))
     (check-roundtrip (hash 1 (list #t 1 msgpack-nil)
                            msgpack-nil #"foo"))))

   (test-suite
    "ext"

    (test-suite
     "timestamp"

     (check-roundtrip (seconds->date 0 #f))
     (check-roundtrip (seconds->date (find-seconds 15 30 12 29 5 2022 #f) #f))
     (check-equal?
      (read-msgpack (open-input-bytes #"\xD7\xFF\x00\x00\x00\x00\x62\x92\xb7\x80"))
      (seconds->date (find-seconds 0 0 0 29 5 2022 #f) #f))
     (check-equal?
      (read-msgpack (open-input-bytes #"\xD6\xFF\x62\x92\xb7\x80"))
      (seconds->date (find-seconds 0 0 0 29 5 2022 #f) #f)))

    (test-suite
     "custom"

     (let ()
       (struct s (bs) #:transparent)
       (parameterize ([current-msgpack-ext-read-handler
                       (λ (t bs)
                         (check-equal? t 1)
                         (s bs))]
                      [current-msgpack-ext-write-handler
                       (λ (v)
                         (values 1 (s-bs v)))])
         (for ([bs (in-list (list #"" #"a" #"ab" #"abcd" (make-bytes 8) (make-bytes 16) (make-bytes 32) (make-bytes (* 1 1024 1024))))])
           (check-roundtrip (s bs)))))))

   (test-suite
    "property tests"

    (check-property prop:roundtrip))))

(module+ test
  (require rackunit/text-ui)
  (run-tests messagepack-suite))
