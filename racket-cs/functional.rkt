;;; creater and owner of this file is @sleepnova, exclude from LICENSE of this project
#lang racket

(define file
  (command-line #:args (filename) filename))

(define (trans in)
  (let next ([ptr 'ptr])
    ;; use `ptr` exactly once
    (match (read-char in)
      [(== eof) ptr]
      [#\+ (next `(inc ,ptr))]
      [#\- (next `(dec ,ptr))]
      [#\> (next `(ptr++ ,ptr))]
      [#\< (next `(ptr-- ,ptr))]
      [#\. (next `(write ,ptr))]
      [#\, (next `(read ,ptr))]
      [#\[ (let ([body (next 'ptr)]
                 [done (next 'ptr)])
             `(let loop ([ptr ,ptr])
                (if (zero? (current-byte ptr))
                    ,done
                    (loop ,body))))]
      [#\] ptr]
      [_ (next ptr)])))

(define code-block
  (trans (open-input-file file)))

(define bf-module
  `(module bf-module racket/base
     (require (rename-in racket/unsafe/ops
                         [unsafe-bytes-ref bytes-ref]
                         [unsafe-bytes-set! bytes-set!]))

     (define-syntax-rule (add1 n) (unsafe-fx+ n 1))
     (define-syntax-rule (sub1 n) (unsafe-fx- n 1))
     (printf "go\n")

     (define (run)
       (define arr (make-bytes 30000 0))

       (define (current-byte ptr) (bytes-ref arr ptr))
       (define (set-current-byte! val ptr) (bytes-set! arr ptr val))

       (define (ptr++ ptr) (add1 ptr))
       (define (ptr-- ptr) (sub1 ptr))
       (define (inc ptr) (set-current-byte! (add1 (current-byte ptr)) ptr) ptr)
       (define (dec ptr) (set-current-byte! (sub1 (current-byte ptr)) ptr) ptr)
       (define (write ptr) (write-byte (current-byte ptr)) ptr)
       (define (read ptr) (set-current-byte! (read-byte) ptr) ptr)

       (void (let ([ptr 0])
               ,code-block))

       (newline))

     (time (run))))

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(eval bf-module ns)
(dynamic-require ''bf-module #f)
