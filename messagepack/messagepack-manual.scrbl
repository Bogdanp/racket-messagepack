#lang scribble/manual

@(require (for-label racket/base
                     racket/contract
                     messagepack))

@title{MessagePack}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[messagepack]

@(define (site . pre-content)
   (apply link "https://msgpack.org/" pre-content))

This module provides an implementation of the @site{MessagePack}
serialization format.  Data is converted between MessagePack and
Racket according to the following table:

@tabular[#:style 'boxed
         #:column-properties '(left right)
         #:row-properties '(bottom-border ())
         (list (list @bold{MsgPack Type}   @bold{Racket Contract})
               (list @tt{nil}              @racket[msgpack-nil?])
               (list @tt{bool}             @racket[boolean?])
               (list @tt{int}              @racket[(integer-in (- (expt 2 63)) (sub1 (expt 2 64)))])
               (list @tt{float}            @racket[real?])
               (list @tt{bin}              @racket[bytes?])
               (list @tt{string}           @racket[string?])
               (list @tt{array}            @racket[list?])
               (list @tt{map}              @racket[hash?])
               (list @tt{timestamp}        @racket[date*?]))]

When serializing integer values to MessagePack, the library chooses
the shortest representation available.

@deftogether[(
  @defproc[(msgpack-nil? [v any/c]) boolean?]
  @defthing[msgpack-nil symbol?]
)]{

  The value used to represent @tt{nil} and its predicate.
}

@defproc[(read-msgpack [in input-port?]) any/c]{
  Reads a MessagePack-encoded value from @racket[in].  This
  implementation doesn't do anything to protect against
  maliciously-constructed data, so care must be taken when reading
  data from untrusted sources.

  Extensions may be handled by parameterizing
  @racket[current-msgpack-ext-read-handler].
}

@defproc[(write-msgpack [v any/c]
                        [out output-port? (current-output-port)]) void?]{

  Writes @racket[v] to @racket[out] as MessagePack-encoded data.

  Extensions may be handled by parameterizing
  @racket[current-msgpack-ext-write-handler].
}

@defparam[current-msgpack-ext-read-handler handler (-> (integer-in 0 127) bytes? any/c) #:value #f]{
  Holds the procedure that is used to read application-specific data.
  The first argument is the type tag and the second is the encoded
  value.

  The default implementation raises an exception when called.
}

@defparam[current-msgpack-ext-write-handler handler (-> any/c (values (integer-in 0 127) bytes?)) #:value #f]{
  Holds the procedure that is used to write application-specific data.
  The argument is the value to encode and the result values are the
  desired type tag and the value encoded as bytes, respectively.

  The default implementation raises an exception when called.
}
