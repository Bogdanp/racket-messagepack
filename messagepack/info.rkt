#lang info

(define license 'BSD-3-Clause)
(define collection "messagepack")
(define deps '("base"
               "messagepack-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
(define implies '("messagepack-lib"))
(define scribblings '(("messagepack-manual.scrbl")))
