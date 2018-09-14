#lang racket

(require (prefix-in s19: (lib "19.ss" "srfi"))
         (prefix-in s48: (lib "48.ss" "srfi"))
         db
         format-numbers
         )

(provide make-transaction transaction-date transaction-order transaction-amount
         transaction-dr transaction-cr transaction-detail
         xlate-sql-spec-chars
         list->transaction
         detail-who
         get-desc
         str-dr-cat
         str-dr-acct
         str-cr-cat
         str-cr-acct
         acct-core
         deductible?
         whose-deduction
         str-starts-with
         balance-account?
         reconciliation-tag
         get-dr-core
         get-cr-core
         dr-cr-amt
         get-year
         get-month
         get-day
         get-db-date
         get-db-dr
         get-db-cr
         get-db-acct
         get-db-payee
         get-db-desc
         who-dr-deduction
         who-cr-deduction
         get-db-dr-deduct
         get-db-cr-deduct
         get-db-dr/cr-seen
         get-db-dr-seen
         get-db-cr-seen
         sqlquot
         append-comma
         make-transaction-sql
         generate-ledger-sql-file)

(define sql-insert-ledger-basename "insert")
(define family-members '(gsk rjdk adk wwk))

(define (ledger-input-dir year)
  (unless (getenv "ME")
    (error "ME must be defined"))
  (string-append (getenv "ME") "/" (number->string year)))

(define (ledger-input-filename year)
  (string-append (ledger-input-dir year) "/ledger"))

(define (ledger-sql-dir year)
  (string-append (ledger-input-dir year) "/sql"))

(define (sql-insert-ledger-filename year)
  (string-append (ledger-sql-dir year) "/" sql-insert-ledger-basename ".sql"))

(define (xlate-sql-spec-chars string)
  (list->string
   (flatten
    (map (lambda (ch)
           (if (equal? ch #\') '(#\' #\') (list ch)))
         (string->list string)))))

(define ledger #f)

(define-struct transaction (date order amount dr cr detail) #:transparent)
(define-struct tr-detail (who what sno) #:transparent)

(define (list->transaction sexp)
  (make-transaction (first sexp)
                    (second sexp)
                    (third sexp)
                    (fourth sexp)
                    (fifth sexp)
                    (if (> (length sexp) 5)
                        (sixth sexp)
                        empty)))

(define (detail-who transaction)
  (let ((x (transaction-detail transaction)))
    (if (empty? x)
        #f
        (first x))))

(define (get-desc transaction)
  (let ((x (transaction-detail transaction)))
    (if (< (length x) 2)
        ""
        (second x))))

(define (str-dr-cat transaction)
  (symbol->string (first (transaction-dr transaction))))

(define (str-dr-acct transaction)
  (symbol->string (second (transaction-dr transaction))))

(define (str-cr-cat transaction)
  (symbol->string (first (transaction-cr transaction))))

(define (str-cr-acct transaction)
  (symbol->string (second (transaction-cr transaction))))

; (x foo [deduct] [who]) -> (x foo)
(define (acct-core L)
  (list (first L) (second L)))

; (x foo [deduct] [who]) -> (deduct gsk), else #f
(define (deductible? L)
  (cond ((null? L) #f)
        ((< (length L) 3) #f)
        (else (member 'deduct (cddr L)))))

; (x foo [deduct] [who]) -> (), gsk, or #f
(define (whose-deduction L)
  (let ((d (deductible? L)))
    (if d
        (let ((w (remove 'deduct d)))
          (if (and (pair? w) (member (car w) family-members))
              (car w)
              empty))
        #f)))

; str-starts-with : str str -> bool
(define (str-starts-with s x)
  (cond ((< (string-length s) (string-length x)) #f)
        (else (string=? x (substring s 0 (string-length x))))))

; ({a,l} foo ...)
(define (balance-account? L)
  (if (pair? L)
      (let ((s (symbol->string (first L))))
        (or (str-starts-with s "a")
            (str-starts-with s "l")))
      #f))

; ({a,l} foo [!] [number])
; returns a string, or #f
(define (reconciliation-tag L)
  (cond ((null? L) #f)
        ((< (length L) 3) #f)
        ((balance-account? L)
         (let ((m (first (reverse L))))
           (cond ((number? m) (subspacezero (s48:format "~2F" m)))
                 ((equal? m '!) #f)
                 ((symbol? m) m)
                 ((string? m) m)
                 (else #f))))
        (else #f)))

(define (get-dr-core transaction)
  (acct-core (transaction-dr transaction)))

(define (get-cr-core transaction)
  (acct-core (transaction-cr transaction)))

(define (dr-cr-amt acct transaction)
  (let ((d (get-dr-core transaction))
        (c (get-cr-core transaction)))
    (cond ((and (equal? acct d)
                (equal? acct c)) 0.)
          ((equal? acct d) (transaction-amount transaction))
          ((equal? acct c) (- 0. (transaction-amount transaction)))
          (else 0.))))

(define (get-year yymmdd)
  (let ((yy (quotient yymmdd 10000)))
    (if (>= yy 60)
        (+ 1900 yy)
        (+ 2000 yy))))
(define (get-month yymmdd) (quotient (modulo yymmdd 10000) 100))
(define (get-day yymmdd) (modulo yymmdd 100))

(define (get-db-date transaction)
  (let* ((yymmdd (transaction-date transaction))
         (date (s19:make-date 0 0 0 0 (get-day yymmdd) (get-month yymmdd) (get-year yymmdd) 0)))
    (s19:date->string date "~Y-~m-~d")))

(define (get-db-dr transaction)
  (get-db-acct (transaction-dr transaction)))

(define (get-db-cr transaction)
  (get-db-acct (transaction-cr transaction)))

(define (get-db-acct acct)
  (string-append (symbol->string (first acct)) " " (symbol->string (second acct))))

(define (get-db-payee transaction)
  (detail-who transaction))

(define (get-db-desc transaction)
  (get-desc transaction))

; assumption: transaction is deductible
(define (who-dr-deduction transaction)
  (let ((who (whose-deduction (transaction-dr transaction))))
    (cond [(empty? who)  "family"]
          [(symbol? who) (symbol->string who)]
          [else #f])))

; assumption: transaction is deductible
(define (who-cr-deduction transaction)
  (let ((who (whose-deduction (transaction-cr transaction))))
    (cond [(empty? who)  "family"]
          [(symbol? who) (symbol->string who)]
          [else #f])))

(define (get-db-dr-deduct transaction)
  (if (deductible? (transaction-dr transaction))
      (who-dr-deduction transaction)
      empty))

(define (get-db-cr-deduct transaction)
  (if (deductible? (transaction-cr transaction))
      (who-dr-deduction transaction)
      empty))

; returns string, or #f
(define (get-db-dr/cr-seen transaction dr/cr)
  (let ((get-dr/cr (cond ((eq? 'dr dr/cr) transaction-dr)
                         ((eq? 'cr dr/cr) transaction-cr)
                         (else (error "expected 'dr or 'cr")))))
    (reconciliation-tag (get-dr/cr transaction))))

(define (get-db-dr-seen transaction)
  (get-db-dr/cr-seen transaction 'dr))

(define (get-db-cr-seen transaction)
  (get-db-dr/cr-seen transaction 'cr))

(define (sqlquot s)
  (if (or (null? s) (false? s))
      "NULL"
      (string-append "'"
                     (cond ((symbol? s) (symbol->string s))
                           ((string? s) (xlate-sql-spec-chars s))
                           (else (error (format "sqlquot: ~a not a symbol or string" s))))
                     "'")))

(define (append-comma s)
  (string-append s ","))

(define (make-transaction-sql transaction)
  (string-append
   "insert into ledger (date,amount,dr_acct,cr_acct,payee,description,dr_seen,cr_seen,dr_deduct,cr_deduct)\n values ("
   
   (append-comma (sqlquot (get-db-date transaction)))
   (append-comma (number->string (transaction-amount transaction)))
   
   (append-comma (sqlquot (get-db-dr transaction)))
   (append-comma (sqlquot (get-db-cr transaction)))
   
   (append-comma (sqlquot (get-db-payee transaction)))
   (append-comma (sqlquot (get-db-desc transaction)))
   
   (append-comma (sqlquot (get-db-dr-seen transaction)))
   (append-comma (sqlquot (get-db-cr-seen transaction)))
   
   (append-comma (sqlquot (get-db-dr-deduct transaction)))
   (sqlquot (get-db-cr-deduct transaction))
   
   ");"))

(define (read-ledger filename)
  (call-with-input-file filename
    (lambda (p)
      (let loop ((sexp (read p))
                 (result '()))
        (if (eof-object? sexp)
            (reverse result)
            (loop (read p) (cons (list->transaction sexp) result)))))))

(define dbg-cur-trans #f)

(define (write-ledger-sql filename transactions)
  (let ((oport (open-output-file filename #:exists 'replace)))
    (for-each (lambda (transaction)
                (set! dbg-cur-trans transaction)
                (fprintf oport "~a\n" (make-transaction-sql transaction)))
              transactions)
    (set! dbg-cur-trans 'done)
    (close-output-port oport)))

(define (generate-ledger-sql-file year)
  (write-ledger-sql (sql-insert-ledger-filename year)
                    (read-ledger (ledger-input-filename year))))
