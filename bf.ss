(import (chezscheme))

(define-record-type op (fields op val))
(define-record-type tape (fields data pos))

;;; Vector and tape ops.

(define (vector-copy! dest dest-start src)
  (define i dest-start)
  (vector-for-each
    (lambda (x)
      (vector-set! dest i x)
      (set! i (+ 1 i)))
    src))

(define (vector-grow-if-needed vec len)
  (if (<= len (vector-length vec))
      vec
      (let ((new-vec (make-vector len)))
        (vector-copy! new-vec 0 vec)
        new-vec)))

(define (tape-get t)
  (vector-ref (tape-data t) (tape-pos t)))

(define (tape-forward t)
  (let ((new-pos (+ 1 (tape-pos t))))
    (make-tape
      (vector-grow-if-needed (tape-data t) (add1 new-pos))
      new-pos)))
 (define (tape-back t)
  (let ((new-pos (- (tape-pos t) 1)))
    (make-tape
      (vector-grow-if-needed (tape-data t) (add1 new-pos))
      new-pos)))

(define (tape-inc! t)
  (let ((data (tape-data t)) (pos (tape-pos t)))
    (vector-set! data pos (+ (vector-ref data pos) 1))
    t))
(define (tape-dec! t)
  (let ((data (tape-data t)) (pos (tape-pos t)))
    (vector-set! data pos (- (vector-ref data pos) 1))
    t))

;;; Parser.

(define (parse-helper lst acc)
  (if (null? lst)
      (reverse acc)
      (let ((rst (cdr lst)))
        (case (car lst)
          ((#\+) (parse-helper rst (cons (make-op 'inc 1) acc)))
          ((#\-) (parse-helper rst (cons (make-op 'dec 1) acc)))
          ((#\>) (parse-helper rst (cons (make-op 'right 1) acc)))
          ((#\<) (parse-helper rst (cons (make-op 'left 1) acc)))
          ((#\.) (parse-helper rst (cons (make-op 'print '()) acc)))
          ((#\[) (let ((subparsed (parse-helper rst '())))
                      (parse-helper
                        (car subparsed)
                        (cons
                          (make-op 'loop (cdr subparsed))
                          acc))))
          ((#\]) (cons rst (reverse acc)))
          (else (parse-helper rst acc))))))

(define (parse bf-code) (parse-helper (string->list bf-code) '()))

;;; Interpreter.
(define (run parsed t)
  (if (null? parsed)
    t
    (let* ([op (op-op (car parsed))]
           [rst (cdr parsed)])
      (case op
        ((inc) (run rst (tape-inc! t)))
        ((dec) (run rst (tape-dec! t)))
        ((right) (run rst (tape-forward t)))
        ((left) (run rst (tape-back t)))
        ((print)
         (display (integer->char (tape-get t)))
         (flush-output-port)
         (run rst t))
        ((loop)
         (if (> (tape-get t) 0)
             (run parsed (run (op-val (car parsed)) t))
             (run rst t)))
        (else (run rst t))))))

;;; I/O.
(define (file->string path)
  (call-with-input-file path
    (lambda (port) (get-string-all port))))

(let* ([cl (command-line)]
       [file (list-ref cl 1)]
       [text (file->string "bench.b")])
  (time (run
    (parse text)
    (make-tape (make-vector 1) 0))))
