#lang racket/base

(require racket/contract
         "private/nil.rkt"
         "private/read.rkt"
         "private/write.rkt")

(provide
 msgpack-nil?
 msgpack-nil
 (contract-out
  [current-msgpack-ext-read-handler (parameter/c (or/c #f (-> (integer-in 0 127) bytes? any/c)))]
  [current-msgpack-ext-write-handler (parameter/c (or/c #f (-> any/c (values (integer-in 0 127) bytes?))))]
  [read-msgpack (-> input-port? any/c)]
  [write-msgpack (case->
                  (-> any/c void?)
                  (-> any/c output-port? void?))]))
