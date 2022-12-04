#lang racket/base

(provide
 msgpack-nil?
 msgpack-nil)

(define (msgpack-nil? v)
  (eq? v msgpack-nil))

(define msgpack-nil
  (string->uninterned-symbol "nil"))
