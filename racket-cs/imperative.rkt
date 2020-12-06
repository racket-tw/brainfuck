;;; creater and owner of this file is @sleepnova, exclude from LICENSE of this project
#lang racket

(define file
  (command-line #:args (filename) filename))

(define (trans in)
  (let next ()
    (match (read-char in)
      [(== eof) null]
      [#\+ `((inc) ,@(next))]
      [#\- `((dec) ,@(next))]
      [#\> `((ptr++) ,@(next))]
      [#\< `((ptr--) ,@(next))]
      [#\. `((write) ,@(next))]
      [#\, `((read) ,@(next))]
      [#\[ `((until (zero? (current-byte))
                    ,@(next))
             ,@(next))]
      [#\] null]
      [_ (next)])))

(define code-block
  (trans (open-input-file file)))

(define bf-module
  `(module bf-module racket/base
     (require (only-in br/cond until))
     (require (rename-in racket/unsafe/ops
                         [unsafe-bytes-ref bytes-ref]
                         [unsafe-bytes-set! bytes-set!]))

     (define-syntax-rule (add1 n) (unsafe-fx+ n 1))
     (define-syntax-rule (sub1 n) (unsafe-fx- n 1))

     (define (run)
       (define arr (make-bytes 30000 0))
       (define ptr 0)

       (define-syntax-rule (current-byte) (bytes-ref arr ptr))
       (define-syntax-rule (set-current-byte! val) (bytes-set! arr ptr val))

       (define-syntax-rule (ptr++) (set! ptr (add1 ptr)))
       (define-syntax-rule (ptr--) (set! ptr (sub1 ptr)))
       (define-syntax-rule (inc) (set-current-byte! (add1 (current-byte))))
       (define-syntax-rule (dec) (set-current-byte! (sub1 (current-byte))))
       (define-syntax-rule (write) (write-byte (current-byte)))
       (define-syntax-rule (read) (set-current-byte! (read-byte)))

       ,@code-block

       (newline))

     (time (run))))

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(eval bf-module ns)
(dynamic-require ''bf-module #f)
