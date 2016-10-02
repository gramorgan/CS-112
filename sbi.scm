#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

;; prints the contents of a hash table for debugging
(define (dump-hash hash)
    (hash-for-each hash
        (lambda (key value)
                (printf "~s : ~s~n" key value))
    ))

;; symbol table conatining functions
(define *function-table* (make-hash))
(define (function-get key)
        (hash-ref *function-table* key))
(define (function-put! key value)
        (hash-set! *function-table* key value))

;; put builtin functions into function table
(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    `(

        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)

     ))

;; symbol table containing variables
(define *variable-table* (make-hash))
(define (variable-get key)
        (hash-ref *variable-table* key))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

;; put e and pi into variable table
(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair)))
    '(

        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)

     ))

;; symbol table containing labels
(define *label-table* (make-hash))
(define (label-get key)
        (hash-ref *label-table* key))
(define (label-put! key value)
        (hash-set! *label-table* key value))

;; holds this file's name
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

;; print the given list of things to stderr and exit
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

;; print the program usage and exit
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

;; builds the label table from the provided program
(define (build-label-table programlist)
    (for-each
        (lambda (line)
            (when (and (> (length line) 1) (symbol? (cadr line)))
                (label-put! (symbol->string (cadr line)) (car line))))
    programlist))

;; read the program into a list
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;; main
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (build-label-table program)
              (dump-hash *label-table*))))

(main (vector->list (current-command-line-arguments)))
