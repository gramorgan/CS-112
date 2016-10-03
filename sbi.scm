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
        (hash-ref *function-table* key #f))
(define (function-put! key value)
        (hash-set! *function-table* key value))

;; fill function table
(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    `(

        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (%       ,(lambda (x y) (- x (* (/ x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (*       ,*)
        (/       ,/)
        (+       ,+)
        (-       ,-)
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
        (hash-ref *variable-table* key #f))
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
        (hash-ref *label-table* key #f))
(define (label-put! key value)
        (hash-set! *label-table* key value))

;; symbol table containing relational operators
(define *relop-table* (make-hash))
(define (relop-get key)
        (hash-ref *relop-table* key #f))
(define (relop-put! key value)
        (hash-set! *relop-table* key value))

;; put operators into relop table
(for-each
    (lambda (pair)
            (relop-put! (car pair) (cadr pair)))
    `(

        (>       '>)
        (<       '>)
        (>=      '>=)
        (<=      '<=)
        (=       '=)
        (<>      '(lambda (a, b) (not (= a, b))))

     ))

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
                (label-put! (cadr line) (- (car line) 1))))
    programlist))

;; read the program into a list
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;; strip the statement from a given line of the program
(define (get-statement-from-line line)
    (case (length line)
        ((1) '(none))
        ((2) 
            (if (symbol? (cadr line))
                '(none)
                (cadr line)))
        ((3) (caddr line))
        (else (die `("invalid line: " ,@line)))))

;; evaluates an expression to get its result
(define (eval-expression expr)
    (cond
        ((number? expr) expr)
        ((symbol? expr) (variable-get expr))
        ((pair? expr)
            (if (variable-get (car expr))
                (vector-ref (variable-get (car expr)) (- (eval-expression (cadr expr)) 1))
                (apply
                    (function-get (car expr))
                    (map eval-expression (cdr expr)))))))

;; print a list in the format required for the print statement
(define (print-list list)
    (if (equal? list '())
        (newline)
        (if (string? (car list))
            (begin
                (display (car list))
                (print-list (cdr list)))
            (begin
                (printf " ~s" (eval-expression (car list)))
                (print-list (cdr list))))))

;; go to the next statement in cur-programlist
(define (next-statement cur-programlist full-programlist)
    (if (equal? (cdr cur-programlist) '())
        (exit 0)
        (interpret-program (cdr cur-programlist) full-programlist)))

;; interpret the sbir program
;; (car cur-programlist) is the current statement being processed
;; full-programlist is always the full list of statements
;;  - used for goto and if
(define (interpret-program cur-programlist full-programlist)
    (let ((statement (get-statement-from-line (car cur-programlist))))
        (case (symbol->string (car statement))
            (("none") (next-statement cur-programlist full-programlist))
            (("dim")
                (variable-put! (caadr statement) (make-vector (eval-expression (cadadr statement))))
                (next-statement cur-programlist full-programlist))
            (("let") 
                (cond
                    ((symbol? (cadr statement))
                        (variable-put! (cadr statement) (eval-expression (caddr statement))))
                    ((pair? (cadr statement))
                        (let* ((args (cadr statement))
                               (array (variable-get (car args))))
                            (vector-set! array (- (eval-expression (cadr args)) 1) (caddr statement))))
                    (else (die `("invalid statement: " ,@statement))))
                (next-statement cur-programlist full-programlist))
            (("goto")
                (interpret-program (list-tail full-programlist (label-get (cadr statement))) full-programlist))
            (("if")
                (let ((conditional (cadr statement)))
                    (if ((relop-get (car conditional)) (cadr conditional) (caddr conditional))
                        (interpret-program (list-tail full-programlist (label-get (caddr statement))) full-programlist)
                        (next-statement cur-programlist full-programlist))))
            (("print")
                (print-list (cdr statement))
                (next-statement cur-programlist full-programlist))
            (("input") (printf "input~n"))
            (else (die `("invalid statement: " ,@statement))))))

;; main
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (build-label-table program)
              (interpret-program program program))))

(main (vector->list (current-command-line-arguments)))
