#lang racket

(require (prefix-in s19: (lib "19.ss" "srfi"))
         (prefix-in s48: (lib "48.ss" "srfi"))
         format-numbers racket/trace
         "../load/load.rkt")

(define ledger-input-filename "sample-ledger")
(define sql-insert-filename "tmp-insert.sql")
(define family-members '(gsk rjdk adk wwk))

(trace-define (xlate-sql-spec-chars string)
  (list->string
   (flatten
    (map (lambda (ch)
           (if (equal? ch #\') '(#\' #\') (list ch)))
         (string->list string)))))

(define ledger #f)

(struct tr-detail (who what sno) #:transparent)

(trace-define (list->transaction sexp)
  (make-transaction (first sexp)
                    (second sexp)
                    (third sexp)
                    (fourth sexp)
                    (fifth sexp)
                    (if (> (length sexp) 5)
                        (sixth sexp)
                        empty)))

(trace-define (detail-who transaction)
  (let ((x (transaction-detail transaction)))
    (if (empty? x)
        #f
        (first x))))

(trace-define (get-desc transaction)
  (let ((x (transaction-detail transaction)))
    (if (< (length x) 2)
        ""
        (second x))))

(trace-define (str-dr-cat transaction)
  (symbol->string (first (transaction-dr transaction))))

(trace-define (str-dr-acct transaction)
  (symbol->string (second (transaction-dr transaction))))

(trace-define (str-cr-cat transaction)
  (symbol->string (first (transaction-cr transaction))))

(trace-define (str-cr-acct transaction)
  (symbol->string (second (transaction-cr transaction))))

; (x foo [deduct] [who]) -> (x foo)
(trace-define (acct-core L)
  (list (first L) (second L)))

; (x foo [deduct] [who]) -> (deduct gsk), else #f
(trace-define (deductible? L)
  (cond ((null? L) #f)
        ((< (length L) 3) #f)
        (else (member 'deduct (cddr L)))))

; (x foo [deduct] [who]) -> (), gsk, or #f
(trace-define (whose-deduction L)
  (let ((d (deductible? L)))
    (if d
        (let ((w (remove 'deduct d)))
          (if (and (pair? w) (member (car w) family-members))
              (car w)
              empty))
        #f)))

; str-starts-with : str str -> bool
(trace-define (str-starts-with s x)
  (cond ((< (string-length s) (string-length x)) #f)
        (else (string=? x (substring s 0 (string-length x))))))

; ({a,l} foo ...)
(trace-define (balance-account? L)
  (if (pair? L)
      (let ((s (symbol->string (first L))))
        (or (str-starts-with s "a")
            (str-starts-with s "l")))
      #f))

; ({a,l} foo [!] [number])
; returns a string, or #f
(trace-define (reconciliation-tag L)
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

(trace-define (get-dr-core transaction)
  (acct-core (transaction-dr transaction)))

(trace-define (get-cr-core transaction)
  (acct-core (transaction-cr transaction)))

(trace-define (dr-cr-amt acct transaction)
  (let ((d (get-dr-core transaction))
        (c (get-cr-core transaction)))
    (cond ((and (equal? acct d)
                (equal? acct c)) 0.)
          ((equal? acct d) (transaction-amount transaction))
          ((equal? acct c) (- 0. (transaction-amount transaction)))
          (else 0.))))

(trace-define (get-year yymmdd)
  (let ((yy (quotient yymmdd 10000)))
    (if (>= yy 60)
        (+ 1900 yy)
        (+ 2000 yy))))
(trace-define (get-month yymmdd) (quotient (modulo yymmdd 10000) 100))
(trace-define (get-day yymmdd) (modulo yymmdd 100))

(trace-define (get-db-date transaction)
  (let* ((yymmdd (transaction-date transaction))
         (date (s19:make-date 0 0 0 0 (get-day yymmdd) (get-month yymmdd) (get-year yymmdd) 0)))
    (s19:date->string date "~Y-~m-~d")))

(trace-define (get-db-dr transaction)
  (get-db-acct (transaction-dr transaction)))

(trace-define (get-db-cr transaction)
  (get-db-acct (transaction-cr transaction)))

(trace-define (get-db-acct acct)
  (string-append (symbol->string (first acct)) " " (symbol->string (second acct))))

(trace-define (get-db-payee transaction)
  (detail-who transaction))

(trace-define (get-db-desc transaction)
  (get-desc transaction))

; assumption: transaction is deductible
(trace-define (who-dr-deduction transaction)
  (let ((who (whose-deduction (transaction-dr transaction))))
    (cond [(empty? who)  "family"]
          [(symbol? who) (symbol->string who)]
          [else #f])))

; assumption: transaction is deductible
(trace-define (who-cr-deduction transaction)
  (let ((who (whose-deduction (transaction-cr transaction))))
    (cond [(empty? who)  "family"]
          [(symbol? who) (symbol->string who)]
          [else #f])))

(trace-define (get-db-dr-deduct transaction)
  (if (deductible? (transaction-dr transaction))
      (who-dr-deduction transaction)
      empty))

(trace-define (get-db-cr-deduct transaction)
  (if (deductible? (transaction-cr transaction))
      (who-dr-deduction transaction)
      empty))

; returns string, or #f
(trace-define (get-db-dr/cr-seen transaction dr/cr)
  (let ((get-dr/cr (cond ((eq? 'dr dr/cr) transaction-dr)
                         ((eq? 'cr dr/cr) transaction-cr)
                         (else (error "expected 'dr or 'cr")))))
    (reconciliation-tag (get-dr/cr transaction))))

(trace-define (get-db-dr-seen transaction)
  (get-db-dr/cr-seen transaction 'dr))

(trace-define (get-db-cr-seen transaction)
  (get-db-dr/cr-seen transaction 'cr))

(trace-define (sqlquot s)
  (if (or (null? s) (false? s))
      "NULL"
      (string-append "'"
                     (cond ((symbol? s) (symbol->string s))
                           ((string? s) (xlate-sql-spec-chars s))
                           (else (error (format "sqlquot: ~a not a symbol or string" s))))
                     "'")))

(trace-define (append-comma s)
  (string-append s ","))

(trace-define (make-transaction-sql transaction)
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

(trace-define (read-ledger filename)
  (call-with-input-file filename
    (lambda (p)
      (let loop ((sexp (read p))
                 (result '()))
        (if (eof-object? sexp)
            (reverse result)
            (loop (read p) (cons (list->transaction sexp) result)))))))

(define dbg-cur-trans #f)

(trace-define (write-ledger-sql filename transactions)
  (let ((oport (open-output-file filename #:exists 'replace)))
    (for-each (lambda (transaction)
                (set! dbg-cur-trans transaction)
                (fprintf oport "~a\n" (make-transaction-sql transaction)))
              transactions)
    (set! dbg-cur-trans 'done)
    (close-output-port oport)))

(trace-define (generate-ledger-sql-file year)
  (write-ledger-sql sql-insert-filename (read-ledger ledger-input-filename)))

(generate-ledger-sql-file 2018)
