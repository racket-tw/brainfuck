(import (chezscheme))

(define file
  (list-ref (command-line) 1))

(define (trans in)
  (let next ([ptr 'ptr])
    ;; use `ptr` exactly once
    (case (read-char in)
      [eof ptr]
      [#\+ (next `(inc ,ptr))]
      [#\- (next `(dec ,ptr))]
      [#\> (next `(ptr++ ,ptr))]
      [#\< (next `(ptr-- ,ptr))]
      [#\. (next `(write/p ,ptr))]
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

(define (run)
  (define mod
    `(module m ()
       (define arr (make-vector 30000))

       (define (current-byte ptr) (vector-ref arr ptr))
       (define (set-current-byte! val ptr) (vector-set! arr ptr val))

       (define (ptr++ ptr) (add1 ptr))
       (define (ptr-- ptr) (sub1 ptr))
       (define (inc ptr) (set-current-byte! (add1 (current-byte ptr)) ptr) ptr)
       (define (dec ptr) (set-current-byte! (sub1 (current-byte ptr)) ptr) ptr)
       (define (write/p ptr) (write (current-byte ptr)) ptr)

       (let ([ptr 0])
         ,code-block
         (newline))))
  (eval mod))

(time (run))
