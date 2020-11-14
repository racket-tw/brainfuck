#lang racket/base

(require racket/file racket/list racket/match racket/cmdline racket/os
         (rename-in racket/unsafe/ops
                    [unsafe-vector-ref vector-ref]
                    [unsafe-vector-set! vector-set!]
                    [unsafe-fx+ +]))

(define-syntax-rule (define-match-expander* k r) (define-match-expander k r r))
(define-match-expander* op (syntax-rules () [(_ op val) (cons op val)]))
(define-match-expander* tape (syntax-rules () [(_ data pos) (mcons data pos)]))

;;; Vector and tape ops.
(define (vector-grow-if-needed vec len)
  (define old-len (vector-length vec))
  (cond [(< len old-len) vec]
        [else
         (let loop ([new-len (* 2 old-len)])
           (cond [(>= len new-len) (loop (* 2 new-len))]
                 [else (define new-vec (make-vector new-len))
                       (vector-copy! new-vec 0 vec)
                       new-vec]))]))

(define (tape-get t)
  (match-let ([(tape data pos) t])
    (vector-ref data pos)))

(define (tape-forward! t)
  (match-let ([(tape data pos) t])
    (let ([new-pos (+ pos 1)])
      (set-mcar! t (vector-grow-if-needed data new-pos))
      (set-mcdr! t new-pos))))
 (define (tape-back! t)
  (match-let ([(tape data pos) t])
    (let ([new-pos (- pos 1)])
      (set-mcar! t (vector-grow-if-needed data new-pos))
      (set-mcdr! t new-pos))))
(define (tape-inc! t)
  (match-let ([(tape data pos) t])
    (vector-set! data pos (+ (vector-ref data pos) 1))))
(define (tape-dec! t)
  (match-let ([(tape data pos) t])
    (vector-set! data pos (- (vector-ref data pos) 1))))

;;; Parser.
(define (parse-helper lst acc)
  (if (empty? lst)
      (reverse acc)
      (let ([rst (rest lst)])
        (match (first lst)
          [#\+ (parse-helper rst (cons 'inc acc))]
          [#\- (parse-helper rst (cons 'dec acc))]
          [#\> (parse-helper rst (cons 'right acc))]
          [#\< (parse-helper rst (cons 'left acc))]
          [#\. (parse-helper rst (cons 'print acc))]
          [#\[ (let ([subparsed (parse-helper rst empty)])
                 (parse-helper (first subparsed)
                               (cons (op 'loop (rest subparsed)) acc)))]
          [#\] (cons rst (reverse acc))]
          [_ (parse-helper rst acc)]))))

(define (parse bf-code) (parse-helper (string->list bf-code) empty))

;;; Interpreter.

(define (run parsed t)
  (define step-op!
    (match-lambda
      ['inc (tape-inc! t)]
      ['dec (tape-dec! t)]
      ['right (tape-forward! t)]
      ['left (tape-back! t)]
      ['print (display (integer->char (tape-get t)))
              (flush-output)]
      [(op 'loop body) (let loop ()
                         (when (> (tape-get t) 0)
                           (step-ops! body)
                           (loop)))]))
  (define step-ops!
    (match-lambda
      [(cons op ops) (step-op! op) (step-ops! ops)]
      [_ (void)]))
  (step-ops! parsed))

(define (read-c path)
  (parameterize ([current-locale "C"])
    (file->string path)))

(module+ main
  (define text null)
  (set! text (read-c (command-line #:args (filename) filename)))

  (run (parse text) (tape (make-vector 30000 0) 0)))
