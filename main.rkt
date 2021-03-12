#lang racket/base

(module reader syntax/module-reader
  racket
  #:read _read
  #:read-syntax _read-syntax
  #:whole-body-readers? #t
  (require file/gunzip net/base64)

  (define (_read port)
    (define np (convert-port port))
    (let loop ()
      (define n (read np))
      (if (eof-object? n) '() (cons n (loop)))))

  (define (_read-syntax src port)
    (define np (convert-port port))
    (let loop ()
      (define n (read-syntax src np))
      (if (eof-object? n) '() (cons n (loop)))))

  (define (convert-port port)
    (read-char port) ;; get the newline following the #lang spec
    (define-values (dl-in dl-out) (make-pipe))
    (define-values (64-in 64-out) (make-pipe))
    (define-values (gz-in gz-out) (make-pipe))
    (thread (λ ()
              (let loop ()
                (define n (read-line port))
                (cond
                  [(eof-object? n) (close-output-port dl-out)]
                  [else (display n dl-out) (loop)]))))
    (thread (λ () (base64-decode-stream dl-in 64-out) (close-output-port 64-out)))
    (thread (λ () (inflate 64-in gz-out) (close-output-port gz-out)))
    (port-count-lines! gz-in)
    gz-in))
