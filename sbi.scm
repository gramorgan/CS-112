#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

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
        (log     ,log)
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (log2    ,(lambda (x) (/ (log x) (log 2.0))))
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
        (round   ,round)
        (log     ,log)
        (sqrt    ,sqrt)
        (cos     ,cos)
        (sin     ,cos)
        (tan     ,tan)
        (acos    ,acos)
        (asin    ,asin)
        (atan    ,atan)
        (abs     ,abs)
        (trunc   ,truncate)

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

;; holds this file's name
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

;; print the given list of things to stderr and exit
(define (die list)
    (for-each (lambda (item) (eprintf "~a " item)) list)
    (newline *stderr*)
    (exit 1)
)

;; print the program usage and exit
(define (usage-exit)
    (die `("Usage:" ,*run-file* "filename"))
)

;; builds the label table from the provided program
(define (build-label-table programlist)
    (for-each
        (lambda (line)
            (when (and (> (length line) 1) (symbol? (cadr line)))
                (label-put! (cadr line) (- (car line) 1))))
    programlist))

;; define the 'not equal' operateo
(define (<> a b)
    (not (= a b)))

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
        (else (die `("invalid line:" ,@line)))))

;; evaluates an expression to get its result
(define (evalexp expr)
    (cond
        ((number? expr) expr)
        ((symbol? expr) (variable-get expr))
        ((pair? expr)
            (if (variable-get (car expr))
                (vector-ref
                    (variable-get (car expr))
                    (- (evalexp (cadr expr)) 1))
                (apply
                    (function-get (car expr))
                    (map evalexp (cdr expr)))))))

(define (get-num-input)
    (let ((input (read)))
        (if (or (number? input) (eof-object? input))
            input
            (begin
                (display "not a number, try again")
                (newline)
                (get-num-input)))))

(define (handle-input args)
    (let take-arg ((args-remaining args)
                   (inputcount -1))
        (if (null? args-remaining)
            inputcount
            (let ((input (get-num-input)))
                (if (eof-object? input)
                    inputcount
                    (begin
                        (variable-put!
                            (car args-remaining)
                            input)
                        (take-arg
                            (cdr args)
                            (if (= inputcount -1)
                                1
                                (+ inputcount 1)))))))))

(define (handle-let args)
    (cond
        ((symbol? (car args))
            (variable-put! (car args) (evalexp (cadr args))))
        ((pair? (car args))
            (vector-set!
                (variable-get (caar args))
                (- (evalexp (cadar args)) 1)
                (cadr args)))
        (else (die `("not a variable" ,(car args))))))

;; print a list in the format required for the print statement
(define (print-list list)
    (if (null? list)
        (newline)
        (if (string? (car list))
            (begin
                (display (car list))
                (print-list (cdr list)))
            (begin
                (printf "~s " (evalexp (car list)))
                (print-list (cdr list))))))

;; macro to go to next statement in list
(define-syntax-rule (next-statement cur-pl full-pl)
    (if (null? (cdr cur-pl))
        (exit 0)
        (interpret-program (cdr cur-pl) full-pl)))

;; interpret the sbir program
;; (car cur-programlist) is the current statement being processed
;; full-programlist is always the full list of statements
;;  - used for goto and if
(define (interpret-program cur-programlist full-programlist)
    (let ((statement (get-statement-from-line (car cur-programlist))))
        (case (symbol->string (car statement))
            (("none") (next-statement cur-programlist full-programlist))
            (("dim")
                (variable-put!
                    (caadr statement)
                    (make-vector (evalexp (cadadr statement))))
                (next-statement cur-programlist full-programlist))
            (("let") 
                (handle-let (cdr statement))
                (next-statement cur-programlist full-programlist))
            (("goto")
                (interpret-program
                    (list-tail full-programlist
                        (label-get (cadr statement)))
                    full-programlist))
            (("if")
                (let ((conditional (cadr statement)))
                    (if ((eval (car conditional))
                            (evalexp (cadr conditional))
                            (evalexp (caddr conditional)))
                        (interpret-program
                            (list-tail full-programlist
                                (label-get (caddr statement)))
                            full-programlist)
                        (next-statement
                            cur-programlist
                            full-programlist))))
            (("print")
                (print-list (cdr statement))
                (next-statement cur-programlist full-programlist))
            (("input")
                (variable-put!
                    'inputcount
                    (handle-input (cdr statement)))
                (next-statement cur-programlist full-programlist))
            (else (die `("invalid statement:" ,@statement))))))

;; main
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (build-label-table program)
              (interpret-program program program))))

(main (vector->list (current-command-line-arguments)))
