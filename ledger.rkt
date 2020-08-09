#lang racket

(require (prefix-in s19: (lib "19.ss" "srfi"))
         (prefix-in s48: (lib "48.ss" "srfi"))
         db
         racket/date
         plot
         format-numbers
         format-ymd
         (file "~/.ledger-dbaccess.rkt")
         (file "~/.ledger-prefs.rkt"))

(provide (all-defined-out))

(plot-new-window? #t)

(define accounts-ht
  (for/hash ([a all-accounts])
    (values (account-id a) a)))

(struct ledger-item (date amount dr-acctid cr-acctid payee description dr-seen cr-seen dr-deduct cr-deduct) #:transparent)
(struct statement-item (acctid date amount description) #:transparent)

(struct ledger-bal-item (ledger-item balance balance-seen diff) #:transparent)

(struct day-bal (date balance) #:transparent)

(define track-item
  (class object%
    (super-new)
    (inspect #f)
    (field [_dr-matched #f]
           [_cr-matched #f])
    (init-field item)
    (define/public (get-item) item)
    (define/public (set-dr-match) (set! _dr-matched #t))
    (define/public (set-cr-match) (set! _cr-matched #t))
    (define/public (dr-matched?) _dr-matched)
    (define/public (dr-unmatched?) (not _dr-matched))
    (define/public (cr-matched?) _cr-matched)
    (define/public (cr-unmatched?) (not _cr-matched))
    (define/public (dr-or-cr-matched?) (or _dr-matched _cr-matched))
    (define/public (neither-dr-nor-cr-matched?) (not (or _dr-matched _cr-matched)))
    ))

(define jan01 (+ (* 10000 start-year) 101))
(define dec31 (+ (* 10000 end-year) 1231))
(define jan0 (- jan01 1))

(define db-source
  (mysql-data-source #:server db-host #:port db-port
                     #:user db-user #:password db-passwd
                     #:database db-schema))

(define (connect!)
  (dsn-connect db-source))

(define the-db
  (virtual-connection (connection-pool connect!)))

;;; Database Calls

(define acct-stmt-balances-ht (make-hash))     ;; all statement balances
(define acct-most-recent-balances-ht (make-hash))  ;; most recent balances

; string -> (list-of (list number number))
(define (db-get-most-recent-actual-balance acctid)
  (if (hash-has-key? acct-most-recent-balances-ht acctid)
      (hash-ref acct-most-recent-balances-ht acctid)
      (let ([answer (let* ([accttype (substring acctid 0 1)]
                           [acctname (substring acctid 2)]
                           [rows (query-rows
                                  the-db
                                  (string-append
                                   "select date, amount from balances where acct_type = '" accttype
                                   "' and acct_name = '" acctname
                                   "' order by date desc limit 1"))])
                      (map (λ (row)
                             (list (sql-date->ymd8 (vector-ref row 0))
                                   (vector-ref row 1)))
                           rows))])
        (hash-set! acct-most-recent-balances-ht acctid answer)
        answer)))

; string -> (list-of (list number number))
(define (db-get-statement-balances acctid)
  (if (hash-has-key? acct-stmt-balances-ht acctid)
      (hash-ref acct-stmt-balances-ht acctid)
      (let ([answer (let* ([accttype (substring acctid 0 1)]
                           [acctname (substring acctid 2)]
                           [rows (query-rows
                                  the-db
                                  (string-append
                                   "select date, amount from balances where acct_type = '" accttype
                                   "' and acct_name = '" acctname
                                   "' and isstmt is true order by date"))])
                      (map (λ (row)
                             (list (sql-date->ymd8 (vector-ref row 0))
                                   (vector-ref row 1)))
                           rows))])
        (hash-set! acct-stmt-balances-ht acctid answer)
        answer)))

(define (closest-statement acctid ymd8)
  (let* ([ds (filter (λ (x) (<= x ymd8)) (get-statement-dates acctid))]
         [ymd8s (member ymd8 ds)])
    (if ymd8s
        (first ymd8s)
        (apply max ds))))

(define (sql-date->ymd8 d)
  (+ (* (sql-date-year d) 10000)
     (* (sql-date-month d) 100)
     (* (sql-date-day d))))

; vector -> statement-item
(define (row-to-statement-item x)
  (statement-item (vector-ref x 1) (sql-date->ymd8 (vector-ref x 2)) (vector-ref x 3) (vector-ref x 4)))

; num num -> (list-of statement-item)
(define (db-get-statement-items start-year end-year)
  (let ([rows (query-rows
               the-db (string-append "select * from statements where date>='" (number->string start-year) "-01-01' and date<='" (number->string end-year) "-12-31' order by date"))])
    (map row-to-statement-item rows)))

(define (db-get-reconciliation-ledger-items acctid ymd8-end statement-balances)
  (let* ([ymd8-start (find-previous-statement-date ymd8-end (reverse statement-balances))]
         [rows (query-rows
                the-db (string-append
                        "select"
                        " id,date,amount,dr_acct,cr_acct,payee,description,dr_seen,cr_seen,dr_deduct,cr_deduct"
                        " from ledger where"
                        " (date >  '" (ymd8->ymd10 ymd8-start) "' and"
                        "  date <= '" (ymd8->ymd10 ymd8-end)   "')"
                        " and"
                        " ((dr_acct = '" acctid "' and (dr_seen is null or dr_seen = ':' )) "
                        "   or "
                        "  (cr_acct = '" acctid "' and (cr_seen is null or cr_seen = ':' )) )"
                        " order by date"))])
    (map row-to-ledger-item rows)))

; string -> (list-of (list number))
(define (db-get-active-apr acctid)
  (let ([rows (query-rows
               the-db
               (string-append
                "(select apr from interest_rates where acct_type='l' and acct_name='"
                (substring acctid 2)
                "' and cur is true)"))])
    (string->number (vector-ref (first rows) 0))))

(define (sub-false-for-sql-null x)
  (if (sql-null? x) #f x))

; vector -> ledger-item
(define (row-to-ledger-item x)
  (ledger-item (sql-date->ymd8 (vector-ref x 1))
               (vector-ref x 2)
               (vector-ref x 3)
               (vector-ref x 4)
               (vector-ref x 5)
               (vector-ref x 6)
               (sub-false-for-sql-null (vector-ref x 7))
               (sub-false-for-sql-null (vector-ref x 8))
               (sub-false-for-sql-null (vector-ref x 9))
               (sub-false-for-sql-null (vector-ref x 10))))

; num num -> (list-of ledger-item)
(define (db-get-ledger-items start-year end-year)
  (let ([rows (query-rows
               the-db (string-append "select id,date,amount,dr_acct,cr_acct,payee,description,dr_seen,cr_seen,dr_deduct,cr_deduct from ledger where date>='" (number->string start-year) "-01-01' and date<='" (number->string end-year) "-12-31' order by date"))])
    (map row-to-ledger-item rows)))

(define all-statement-items empty)
(define all-ledger-items empty)

(define (set-all-statement-items!)
  (set! all-statement-items (db-get-statement-items start-year end-year)))

(define (set-all-ledger-items!)
  (set! all-ledger-items (db-get-ledger-items start-year end-year)))

(define (refresh)
  (set-all-statement-items!)
  (set-all-ledger-items!)
  (hash-clear! acct-stmt-balances-ht)
  (hash-clear! acct-most-recent-balances-ht))

(refresh)

;;; End Database Calls

(define (get-book-ext-diff acctid ymd8)
  (let* ([xs (get-ledger-bal-items acctid)]
         [bals (acct-book-and-ext-balances-on-date acctid ymd8)]
         [book (first bals)]
         [ext (second bals)]
         [book-minus-ext (- book ext)])
    (list book ext book-minus-ext)))

(define (format-ledger-bal-item acctid lbi want-diff-seen?)
  (if want-diff-seen?
      (format "~a ~a ~a ~a"
              (~a (format-exact (ledger-bal-item-diff lbi) 2) #:min-width 9 #:align 'right)
              (~a (format-exact (ledger-bal-item-balance-seen lbi) 2) #:min-width 9 #:align 'right)
              (~a (format-exact (ledger-bal-item-balance lbi) 2) #:min-width 9 #:align 'right)
              (format-ledger-item acctid (ledger-bal-item-ledger-item lbi)))
      (format "~a ~a"
              (~a (format-exact (ledger-bal-item-balance lbi) 2) #:min-width 9 #:align 'right)
              (format-ledger-item acctid (ledger-bal-item-ledger-item lbi)))))

(define (pr-acct-book-and-ext-balances-up-through acctid up-through)
  (for-each
   (λ (lbi)
     (printf "~a" (format-ledger-bal-item acctid lbi #t)))
   (acct-book-and-ext-balances-up-to-date acctid up-through)))

(define (pr-acct-book-and-ext-balances-between-dates acctid from through)
  (for-each
   (λ (lbi)
     (printf "~a" (format-ledger-bal-item acctid lbi #t)))
   (acct-book-and-ext-balances-between-dates acctid from through)))

(define (pr-acct-book-balances-up-through acctid up-through)
  (for-each
   (λ (lbi)
     (printf "~a" (format-ledger-bal-item acctid lbi #f)))
   (acct-book-and-ext-balances-up-to-date acctid up-through)))

(define (pr-acct-book-balances-between-dates acctid from through)
  (for-each
   (λ (lbi)
     (printf "~a" (format-ledger-bal-item acctid lbi #f)))
   (acct-book-and-ext-balances-between-dates acctid from through)))

(define (acct-book-and-ext-balances-up-to-date acctid through)
  (filter (λ (lbi)
            (<= (ledger-item-date (ledger-bal-item-ledger-item lbi)) through))
          (get-ledger-bal-items acctid)))

(define (acct-book-and-ext-balances-between-dates acctid from through)
  (filter (λ (lbi)
            (let ([li-date (ledger-item-date (ledger-bal-item-ledger-item lbi))])
              (and (>= li-date from)
                   (<= li-date through))))
          (get-ledger-bal-items acctid)))

(define (acct-book-and-ext-balances-on-date acctid as-of)
  (let* ([xs (get-ledger-bal-items acctid)]
         [ys (filter (λ (x)
                       (<= (ledger-item-date (ledger-bal-item-ledger-item x)) as-of))
                     xs)])
    (if (empty? ys)
        (list 0 0)
        (list (ledger-bal-item-balance (first (reverse ys)))
              (ledger-bal-item-balance-seen (first (reverse ys)))))
    ))

(define (pr-acct-book-and-ext-balances-on-date acctid as-of)
  (let* ([ls (acct-book-and-ext-balances-on-date acctid as-of)]
         [book (first ls)]
         [ext (second ls)]
         [diff-book-ext (- book ext)]
         [ymd-mr (db-get-most-recent-actual-balance acctid)])
    (printf "~a ~a ~a~a\n"
            (exact->inexact book)
            (exact->inexact ext)
            (exact->inexact diff-book-ext)
            (if (not (empty? ymd-mr))
                (string-append " (" (number->string (first (first ymd-mr)))
                               " " (format-exact (second (first ymd-mr)) 2)
                               " " (format-exact (- (second (first ymd-mr)) ext) 2) ")")
                ""))))

; to return a number ymd8 must be one of the ymd8 values in the list
; (listof (list ymd8 exact)) ymd8 -> exact
(define (get-stmt-bal-for-date date-bals ymd8)
  (cond [(null? date-bals) (error "balance unavailable for date" ymd8)]
        [(= (first (first date-bals)) ymd8) (second (first date-bals))]
        [else (get-stmt-bal-for-date (rest date-bals) ymd8)]))

; string -> (list-of number)
(define (get-statement-dates acctid)
  (map first (db-get-statement-balances acctid)))

; string number number -> (listof number number number)
(define (get-new-dr-new-cr-reconciliation acctid stmt-ymd8)
  (let* ([statement-balances (db-get-statement-balances acctid)]
         [stmt-bal (get-stmt-bal-for-date statement-balances stmt-ymd8)]
         [reconciliation-items (db-get-reconciliation-ledger-items acctid stmt-ymd8 statement-balances)]
         [new-dr (sum-amounts-new-dr acctid stmt-ymd8 reconciliation-items)]
         [new-cr (sum-amounts-new-cr acctid stmt-ymd8 reconciliation-items)]
         [reconciliation (+ stmt-bal new-dr new-cr)])
    (list new-dr new-cr reconciliation)))

; string number number -> (listof number number number number)
(define (get-new-dr-new-cr-reconciliation-diff acctid stmt-ymd8 amt)
  (let* ([dr-cr-rec (get-new-dr-new-cr-reconciliation acctid stmt-ymd8)]
         [new-dr (first dr-cr-rec)]
         [new-cr (second dr-cr-rec)]
         [reconciliation (third dr-cr-rec)]
         [amt-minus-reconciliation (- amt reconciliation)])
    (list new-dr new-cr reconciliation amt-minus-reconciliation)))

; string -> (listof (list number number number number number))
(define (get-reconciliations acctid)
  (map (λ (ymd8-bal)
         (cons (first ymd8-bal) (get-new-dr-new-cr-reconciliation-diff acctid (first ymd8-bal) (second ymd8-bal))))
       (db-get-statement-balances acctid)))

(define (find-previous-statement-date ymd8 statement-balances-going-back-in-time)
  (cond [(empty? statement-balances-going-back-in-time) jan01] ;FIXME needs to be > dec31 not jan01
        [else (let ([x (first (first statement-balances-going-back-in-time))])
                (if (< x ymd8)
                    x
                    (find-previous-statement-date ymd8 (rest statement-balances-going-back-in-time))))]))

(define (first-ledger-unseen-discrepancy acctid)
  (let ([lbis (ledger-unseen-discrepancies acctid)])
    (if (empty? lbis)
        #f
        (let ([lbi (first lbis)])
          (list (- (ledger-bal-item-balance-seen lbi) (ledger-bal-item-balance lbi)) lbi)))))

(define (ledger-unseen-discrepancies acctid)
  (filter (λ (x)
            (not (= (ledger-bal-item-balance x)
                    (ledger-bal-item-balance-seen x))))
          (get-ledger-bal-items acctid)))

(define (ledger-unseen-discrepancy-changes acctid)
  (define (helper acctid ins outs running-discrepancy)
    (if (empty? ins)
        outs
        (let ([new-diff (ledger-bal-item-diff (first ins))])
          (if (= new-diff running-discrepancy)
              (helper acctid (rest ins) outs running-discrepancy)
              (helper acctid (rest ins) (cons (first ins) outs) new-diff)))))
  (reverse (helper acctid (ledger-unseen-discrepancies acctid) empty 0)))

(define (statement-acct-matches acctid statement-items)
  (filter (λ (srow) (string=? (statement-item-acctid srow) acctid)) statement-items))

;(define (ledger-acct acctid ledger-items)
;  (filter (λ (lrow)
;            (or (string=? (ledger-item-dr-acctid lrow) acctid)
;                (string=? (ledger-item-cr-acctid lrow) acctid)))
;          ledger-items))

(define (statement<=ymd8 ymd8 statement-items)
  (filter (λ (si) (<= (statement-item-date si) ymd8)) statement-items))

(define (statement>=ymd8 ymd8 statement-items)
  (filter (λ (si) (>= (statement-item-date si) ymd8)) statement-items))

(define (statement-range ymd8-a ymd8-b statement-items)
  (filter (λ (si)
            (and (>= (statement-item-date si) ymd8-a) (<= (statement-item-date si) ymd8-b)))
          statement-items))

(define (ledger<=ymd8 ymd8 ledger-items)
  (filter (λ (li) (<= (ledger-item-date li) ymd8)) ledger-items))

(define (ledger>=ymd8 ymd8 ledger-items)
  (filter (λ (li) (>= (ledger-item-date li) ymd8)) ledger-items))

(define (ledger-range ymd8-a ymd8-b ledger-items)
  (filter (λ (li)
            (and (>= (ledger-item-date li) ymd8-a) (<= (ledger-item-date li) ymd8-b)))
          ledger-items))

(define (ledger-acct-matches acctid ledger-items)
  (filter (λ (li) (ledger-acct-match acctid li)) ledger-items))

(define (ledger-acct-match acctid ledger-item)
  (or (string=? acctid (ledger-item-dr-acctid ledger-item))
      (string=? acctid (ledger-item-cr-acctid ledger-item))))

(define (ledger-range-acct acctid ymd8-a ymd8-b ledger-items)
  (filter (λ (li)
            (ledger-acct-match acctid li))
          (ledger-range ymd8-a ymd8-b (ledger-acct-matches acctid ledger-items))))

(define (ledger-range-acct-dr acctid ymd8-a ymd8-b ledger-items)
  (filter (λ (li)
            (string=? acctid (ledger-item-dr-acctid li)))
          (ledger-range ymd8-a ymd8-b (ledger-acct-matches acctid ledger-items))))

(define (ledger-range-acct-cr acctid ymd8-a ymd8-b ledger-items)
  (filter (λ (li)
            (string=? acctid (ledger-item-cr-acctid li)))
          (ledger-range ymd8-a ymd8-b (ledger-acct-matches acctid ledger-items))))

(define (ledger-range-signed-amounts acctid ymd8-a ymd8-b ledger-items)
  (map (λ (li)
         (ledger-signed-amount acctid li))
       (ledger-range ymd8-a ymd8-b (ledger-acct-matches acctid ledger-items))))

(define (ledger-range-signed-amounts-seen acctid ymd8-a ymd8-b ledger-items)
  (map (λ (li)
         (ledger-signed-amount-seen acctid li))
       (ledger-range ymd8-a ymd8-b (ledger-acct-matches acctid ledger-items))))

(define (ledger-range-signed-amounts-unseen acctid ymd8-a ymd8-b ledger-items)
  (map (λ (li)
         (ledger-signed-amount-unseen acctid li))
       (ledger-range ymd8-a ymd8-b (ledger-acct-matches acctid ledger-items))))

(define (sum-ledger-range-signed-amounts acctid ymd8-a ymd8-b ledger-items)
  (foldl + 0 (ledger-range-signed-amounts acctid ymd8-a ymd8-b ledger-items)))

(define (sum-ledger-range-signed-amounts-seen acctid ymd8-a ymd8-b ledger-items)
  (foldl + 0 (ledger-range-signed-amounts-seen acctid ymd8-a ymd8-b ledger-items)))

(define (sum-ledger-range-signed-amounts-unseen acctid ymd8-a ymd8-b ledger-items)
  (foldl + 0 (ledger-range-signed-amounts-unseen acctid ymd8-a ymd8-b ledger-items)))

(define (ledger-amount-dr acctid a-ledger-item)
  (if (string=? acctid (ledger-item-dr-acctid a-ledger-item))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amount-cr acctid a-ledger-item)
  (if (string=? acctid (ledger-item-cr-acctid a-ledger-item))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amount-dr-seen acctid a-ledger-item)
  (if (and (ledger-item-dr-seen a-ledger-item)
           (string=? acctid (ledger-item-dr-acctid a-ledger-item)))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amount-cr-seen acctid a-ledger-item)
  (if (and (ledger-item-cr-seen a-ledger-item)
           (string=? acctid (ledger-item-cr-acctid a-ledger-item)))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amount-dr-unseen acctid a-ledger-item)
  (if (and (not (ledger-item-dr-seen a-ledger-item))
           (string=? acctid (ledger-item-dr-acctid a-ledger-item)))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amount-cr-unseen acctid a-ledger-item)
  (if (and (not (ledger-item-cr-seen a-ledger-item))
           (string=? acctid (ledger-item-cr-acctid a-ledger-item)))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-signed-amount acctid a-ledger-item)
  (let ([d (account-division acctid)])
    (cond [(eq? d 'balance-sheet)
           (- (ledger-amount-dr acctid a-ledger-item) (ledger-amount-cr acctid a-ledger-item))]
          [(eq? d 'income-statement)
           (- (ledger-amount-cr acctid a-ledger-item) (ledger-amount-dr acctid a-ledger-item))]
          [else 0])))

(define (account-division acctid)
  (let ([ltr (string-ref acctid 0)])
    (cond [(or (eq? ltr #\a) (eq? ltr #\l) (eq? ltr #\e)) 'balance-sheet]
          [(or (eq? ltr #\r) (eq? ltr #\x))               'income-statement]
          [else #f])))

(define (ledger-signed-amount-seen acctid a-ledger-item)
  (let ([ltr (string-ref acctid 0)])
    (cond [(or (eq? ltr #\a) (eq? ltr #\l) (eq? ltr #\e))
           (- (ledger-amount-dr-seen acctid a-ledger-item)
              (ledger-amount-cr-seen acctid a-ledger-item))]
          [(or (eq? ltr #\r) (eq? ltr #\x))
           (- (ledger-amount-cr-seen acctid a-ledger-item)
              (ledger-amount-dr-seen acctid a-ledger-item))]
          [else 0])))

(define (ledger-signed-amount-unseen acctid a-ledger-item)
  (let ([ltr (string-ref acctid 0)])
    (cond [(or (eq? ltr #\a) (eq? ltr #\l) (eq? ltr #\e))
           (- (ledger-amount-dr-unseen acctid a-ledger-item) (ledger-amount-cr-unseen acctid a-ledger-item))]
          [(or (eq? ltr #\r) (eq? ltr #\x))
           (- (ledger-amount-cr-unseen acctid a-ledger-item) (ledger-amount-dr-unseen acctid a-ledger-item))]
          [else 0])))

; string -> (listof ledger-bal-item)
(define (get-ledger-bal-items acctid)
  (get-ledger-bal-items-from acctid all-ledger-items 0 0))

(define (closing-bal-each-day day-bals)
  (define (dotted-pair-to-day-bal x) (day-bal (car x) (cdr x)))
  (let* ([ht (for/hash ([i day-bals])
               (values (day-bal-date i) (day-bal-balance i)))]
         [dotted-pairs (sequence->list (in-hash-pairs ht))])
    (map
     dotted-pair-to-day-bal
     (sort
      dotted-pairs
      (λ (a b) (< (car a) (car b)))))))

(define (get-day-bals acctid)
  (closing-bal-each-day
   (map (λ (lbi)
          (let ([li (ledger-bal-item-ledger-item lbi)])
            (day-bal (ledger-item-date li) (ledger-bal-item-balance lbi))))
        (get-ledger-bal-items acctid))))

(define (get-day-bals-filter acctid filter-func)
  (closing-bal-each-day
   (map (λ (lbi)
          (let ([li (ledger-bal-item-ledger-item lbi)])
            (day-bal (ledger-item-date li) (ledger-bal-item-balance lbi))))
        (filter filter-func (get-ledger-bal-items acctid)))))

(define (day-bals-from acctid start-ymd8)
  (define filter-func
    (λ (lbi)
      (>= (ledger-item-date (ledger-bal-item-ledger-item lbi)) start-ymd8)))
  (get-day-bals-filter acctid filter-func))

(define (accounts-bals-range accounts start-ymd8 end-ymd8)
  (map (λ (x)
         (list x (day-bals-range x start-ymd8 end-ymd8)))
       (map (λ (a) (account-id a)) accounts)))

(define (day-bals-range acctid start-ymd8 end-ymd8)
  (define filter-func
    (λ (lbi)
      (and (>= (ledger-item-date (ledger-bal-item-ledger-item lbi)) start-ymd8)
           (<= (ledger-item-date (ledger-bal-item-ledger-item lbi)) end-ymd8))))
  (let ([bals (get-day-bals-filter acctid filter-func)])
    (if (and (not (empty? bals))
             (> (day-bal-date (first bals)) start-ymd8))
        (append (day-bals-on acctid start-ymd8) bals)
        bals)))

(define (day-bals-ons acctid ymd8s)
  (map (λ (ymd8)
         (day-bals-on acctid ymd8))
       ymd8s))

(define (day-bals-on acctid ymd8)
  (define filter-func
    (λ (lbi)
      (= (ledger-item-date (ledger-bal-item-ledger-item lbi)) ymd8)))
  (let ([bals (get-day-bals-filter acctid filter-func)])
    (if (empty? bals)
        (list (first (reverse (day-bals-range acctid jan01 ymd8))))
        bals)))

(define (plot-day-bals-from an-account start-ymd8)
  (plot-day-bals an-account (day-bals-from (account-id an-account) start-ymd8)))

(define (plot-day-bals-range an-account start-ymd8 end-ymd8)
  (plot-day-bals an-account (day-bals-range (account-id an-account) start-ymd8 end-ymd8)))

(define (plot-day-bals-forward an-account ndays)
  (plot-day-bals-range an-account (today->ymd8) (ymd8-plusdays->ymd8 (today->ymd8) ndays)))

(define (plot-day-bals an-account bals)
  (let* ([xs (map (λ (b) (date->seconds (ymd8->date (day-bal-date b)))) bals)]
         [ys (map day-bal-balance bals)])
    (parameterize ([plot-title (account-id an-account)]
                   [plot-x-label "Date"]
                   [plot-x-ticks (date-ticks)]
                   [plot-y-label "Amount"])
      (plot (lines (map vector xs ys) #:color (account-color an-account))))))

(define (plot-account-day-bals-forward account ndays)
  (plot-accounts-day-bals-forward (list account) ndays))

(define (plot-accounts-day-bals-forward accounts ndays)
  (plot-accounts-day-bals-range accounts
                                (today->ymd8)
                                (ymd8-plusdays->ymd8 (today->ymd8) ndays)))

(struct ayb (v-titles h-indices h-ymd8s) #:transparent)

; IN: e.g., output from accounts-bals-range
; ( ("acct1" ( (date1 bal1) (date2 bal2) ... ))
;   ("acct2" ( (date1 bal1) (date2 bal2) ... )) ... )
;
; v-titles ->  #("acct1" "acct2" ... "total")
;
; OUT:
; (ayb accts-ymd8s-bals
; h-ymd8s
;    |
;    v
; 20190101 -> #(1 2 ... 3)
; 20190102 -> #(2 5 ... 8)
; ...
; 20191129 -> #(1000 2000 ... 3000)
(define (combine-day-bal-lists lo-acct-lo-day-bals start-ymd8 end-ymd8)
  (let* ([titles0 (map first lo-acct-lo-day-bals)]
         [titles (append titles0 (list "total"))]
         ; #("acct1" "acct2" ... "acctN", "total")
         [v-titles (list->vector titles)]
         ; "acct1" -> 0, "acct2" -> 1, ..., "accnN" -> N-1, "total" -> N
         [h-indices (for/hash ([k titles]
                               [i (in-range (vector-length v-titles))])
                      (values k i))]
         [ymd8s (every-ymd8-from-through start-ymd8 end-ymd8)]
         [h-ymd8s (for/hash ([k ymd8s])
                    (values k (make-vector (vector-length v-titles) #f)))])
    (for-each (λ (acct-lo-day-bals)
                (set-matrix-point-balances acct-lo-day-bals h-ymd8s h-indices start-ymd8))
              lo-acct-lo-day-bals)
    (calculate-matrix-balances lo-acct-lo-day-bals h-ymd8s h-indices)
   (ayb v-titles h-indices h-ymd8s)))

(define (set-matrix-point-balances acct-lo-day-bals h-ymd8s h-indices start-ymd8)
  (let ([acct (first acct-lo-day-bals)]
        [lo-day-bals (second acct-lo-day-bals)])
    ; set the specific balances in the matrix
    (for-each (λ (a-day-bal)
                (let ([ymd8 (day-bal-date a-day-bal)]
                      [bal (day-bal-balance a-day-bal)]
                      [i-acct (hash-ref h-indices acct)])
                  (when (and (or (hash-has-key? h-ymd8s ymd8)
                                 (< ymd8 start-ymd8))
                             (or (false? (hash-ref h-ymd8s ymd8))
                                 (false? (vector-ref (hash-ref h-ymd8s ymd8) i-acct))))
                    (when (false? (hash-ref h-ymd8s ymd8))
                      (hash-set! h-ymd8s ymd8 (make-vector (vector-length h-indices) #f)))
                    (vector-set! (hash-ref h-ymd8s (if (< ymd8 start-ymd8)
                                                       start-ymd8
                                                       ymd8))
                                 i-acct bal))))
              lo-day-bals)))

;; ---------------------------------------------------------------------
;;                 BEFORE            ||             AFTER
;; ---------------------------------------------------------------------
;;       MATRIX      |    MEMORY    |DAY|    MEMORY   |      MATRIX
;; ------------------+--------------|---|-------------------------------
;;   .   12  .   .   |  .   .   .   | 1 |  .   12  .  |  .   12  .    12   
;;   .   .   23  .   |  .   12  .   | 2 |  .   12  23 |  .   12  23   35   
;;   .   32  33  .   |  .   12  23  | 3 |  .   32  33 |  .   32  33   65
;;   .   .   .   .   |  .   32  33  | 4 |  .   32  33 |  .   32  33   65    
;;   51  .   .   .   |  .   32  33  | 5 |  51  32  33 |  51  32  33  116
;;   .   62  .   .   |  51  32  33  | 6 |  51  62  33 |  51  62  33  146
;;   71  .   73  .   |  51  62  33  | 7 |  71  62  73 |  71  62  73  206
;; 
;; foreach day:
;;   matrix[day][total] = 0
;;   foreach acct:
;;     when matrix[day][acct] has no value AND
;;          memory[acct] has a value:
;;       matrix[day][acct] = memory[acct]
;;   foreach acct:
;;     when matrix[day][acct] has a value:
;;       memory[acct] = matrix[day][account]
;;       matrix[day][total] += matrix[day][account]

(define (calculate-matrix-balances lo-acct-lo-day-bals h-ymd8s h-indices)
  (let* ([accts (map first lo-acct-lo-day-bals)]
         [memory (make-vector (hash-count h-indices) #f)]
         [i-total (hash-ref h-indices "total")]
         [ymd8s-sorted (sort (hash-keys h-ymd8s) <)])
    (for-each
     (λ (ymd8)
       (let ([v-today (hash-ref h-ymd8s ymd8)])
         (vector-set! v-today i-total 0)
         (for-each (λ (acct)
                     (let ([i-acct (hash-ref h-indices acct)])
                       (when (and (false? (vector-ref v-today i-acct))
                                  (vector-ref memory i-acct))
                         (vector-set! v-today i-acct (vector-ref memory i-acct)))))
                   accts)
         (for-each (λ (acct)
                     (let* ([i-acct (hash-ref h-indices acct)]
                            [prev-total (vector-ref v-today i-total)])
                       (vector-set! memory i-acct (vector-ref v-today i-acct))
                       (vector-set! v-today i-total (+ prev-total (vector-ref v-today i-acct)))))
                   accts)))
     ymd8s-sorted)
    h-ymd8s))

(define (day-bals-hash->sorted-list h)
  (sort (λ (x y)
          (< (day-bal-date x) (day-bal-date y)))
        (hash-values h)))

(define (pr-min-acct-day-bal-forward acctid ndays)
  (let ([b (min-acct-day-bal-forward acctid ndays)])
    (printf "~a ~a\n" (day-bal-date b) (format-exact (day-bal-balance b) 2))))

(define (min-acct-day-bal-forward acctid ndays)
  (define (helper min-seen answer xs)
    (cond [(empty? xs) answer]
          [(< (day-bal-balance (first xs)) min-seen)
           (helper (day-bal-balance (first xs))
                   (first xs)
                   (rest xs))]
          [else 
           (helper min-seen
                   answer
                   (rest xs))]))
  (helper +inf.0
          #f
          (day-bals-range acctid
                          (today->ymd8)
                          (ymd8-plusdays->ymd8 (today->ymd8) ndays))))
          
(define (pr-outlook-forward accts ndays want-outflow?)
  (let ([d (today->ymd8)])
    (for-each (λ (a)
                (printf "~n===== account: ~a on ~a =====~n~nledger ext diff (most-recent):~n" a d)
                (pr-acct-book-and-ext-balances-on-date a d)
                (printf "~nmininum balance next ~a days:~n" ndays)
                (pr-min-acct-day-bal-forward a ndays)
                (printf "~noutstanding ledger items (~a):~n" a)
                (pr-outstanding-ledger-items a d)
                (printf "~n")
                (when want-outflow?
                  (begin (printf "outflow (~a):~n" a)
                         (pr-outflow-forward a d)
                         (printf "~n"))))
              accts)))

(define (pr-outflow-forward acct ndays)
  (let* ([rx #rx"UNSCHED"]
         [d0 (today->ymd8)]
         [d1 (ymd8-plusdays->ymd8 d0 ndays)]
         [lis (filter (λ (li)
                        (and (<= (ledger-item-date li) d1)
                             (string=? acct (ledger-item-cr-acctid li))
                             (or (regexp-match rx (ledger-item-description li))
                                 (false? (ledger-item-cr-seen li)))))
                      all-ledger-items)]
         [total 0.0])
    (for-each (λ (li)
                (set! total (+ total (ledger-item-amount li)))
                (printf "~a ~a ~a ~a / ~a\n"
                        (~a (format-exact total 2)
                            #:min-width 9 #:align 'right)
                        (ledger-item-date li)
                        (~a (format-exact (ledger-item-amount li) 2)
                            #:min-width 9 #:align 'right)
                        (ledger-item-payee li)
                        (ledger-item-description li)))
              lis)))

(define (plot-accounts-day-bals-range accounts start-ymd8 end-ymd8)
  (parameterize ([plot-x-label "Date"]
                 [plot-x-ticks (date-ticks)]
                 [plot-y-label "Amount"])
    (plot (map (λ (an-account)
                 (lines-acct-day-bals-range an-account start-ymd8 end-ymd8))
               accounts))))

(define (lines-acct-day-bals-range an-account start-ymd8 end-ymd8)
  (let* ([bals (day-bals-range (account-id an-account) start-ymd8 end-ymd8)]
         [xs (map (λ (b) (date->seconds (ymd8->date (day-bal-date b)))) bals)]
         [ys (map day-bal-balance bals)])
    (lines (map vector xs ys) #:color (account-color an-account))))

(define (plot-combined-accounts-day-bals-range accounts start-ymd8 end-ymd8)
  (let* ([lo-acct-lo-day-bals (accounts-bals-range accounts start-ymd8 end-ymd8)]
         [an-ayb (combine-day-bal-lists lo-acct-lo-day-bals start-ymd8 end-ymd8)]
         [accounts+ (append accounts (list (account "total" "black")))])
    (parameterize ([plot-x-label "Date"]
                   [plot-x-ticks (date-ticks)]
                   [plot-y-label "Amount"])
      (plot (lines-combined-accounts-day-bals-range an-ayb accounts+ start-ymd8 end-ymd8)))))

(struct ymd8-x (ymd8 seconds))

(define (lines-combined-accounts-day-bals-range an-ayb accounts start-ymd8 end-ymd8)
  (let* ([h-ymd8s (ayb-h-ymd8s an-ayb)]
         [sorted-ymd8-xs (map (λ (ymd8)
                                (ymd8-x ymd8 (date->seconds (ymd8->date ymd8))))
                              (sort (hash-keys h-ymd8s) <))])
    (map (λ (an-account)
           (let* ([xs (map ymd8-x-seconds sorted-ymd8-xs)]
                  [h-indices (ayb-h-indices an-ayb)]
                  [i-acct (hash-ref h-indices (account-id an-account))]
                  [ys (map (λ (a-ymd8-x)
                             (let* ([ymd8 (ymd8-x-ymd8 a-ymd8-x)]
                                    [v (hash-ref h-ymd8s ymd8)])
                               (vector-ref v i-acct)))
                           sorted-ymd8-xs)])
             (lines (map vector xs ys) #:color (account-color an-account))))
         accounts)))

(define (acct-color acctid)
  (let ([ch (string-ref acctid 0)])
    (cond [(eq? ch #\a) "Forest Green"]
          [(eq? ch #\l) "red"]
          [else "blue"])))

; find first ledger-bal-item where book and ext differ
; string -> (or/c ledger-bal-item #f)
(define (find-first-ledger-bal-item-book-ext-difference acctid)
  (let* ([items (get-ledger-bal-items acctid)]
         [nz-diffs (filter (λ (x) (not (zero? (ledger-bal-item-diff x)))) items)])
    (if (> (length nz-diffs) 0)
        (first nz-diffs)
        #f)))

(define (get-ledger-bal-items-from
         acctid ledger-items starting-balance starting-balance-seen)
  (define (helper ins outs running-balance running-balance-seen)
    (if (empty? ins)
        outs
        (let* ([in (first ins)]
               [signed-amount (ledger-signed-amount acctid in)]
               [signed-amount-seen (ledger-signed-amount-seen acctid in)]
               [newbal (+ running-balance signed-amount)]
               [newbal-seen (+ running-balance-seen signed-amount-seen)])
          (helper (rest ins)
                  (cons (ledger-bal-item in newbal newbal-seen (- newbal newbal-seen)) outs)
                  newbal
                  newbal-seen))))
  (let ([ins (ledger-acct-matches acctid ledger-items)])
    (reverse (helper ins empty starting-balance starting-balance-seen))))

(define (check-ledger-statements-match statement-dates acctid lti sti)
  (for-each (λ (stmt-ymd8)
              (check-ledger-statement-match stmt-ymd8 acctid lti sti))
            statement-dates))

(define (check-ledger-statement-match stmt-ymd8 acctid lti sti)
  (let* ([li (send lti get-item)]
         [si (send sti get-item)]
         [acct-amount-match (categorize-ledger-statement-match-amount acctid li si)])
    (when (and acct-amount-match
               (is-ledger-statement-match-date li si)
               (does-ledger-tag-match-statement acctid stmt-ymd8 li acct-amount-match)
               (is-special-ledger-tag-match acctid li acct-amount-match))
      (cond [(symbol=? acct-amount-match 'acct-amount-dr-and-cr-match)
             (begin
               (send lti set-dr-match)
               (send lti set-cr-match)
               (send sti set-dr-match)
               (send sti set-dc-match)
               'acct-amount-dr-and-cr-match)]
            [(symbol=? acct-amount-match 'acct-amount-dr-matches)
             (begin
               (send lti set-dr-match)
               (send sti set-dr-match)
               'acct-amount-dr-matches)]
            [(symbol=? acct-amount-match 'acct-amount-cr-matches)
             (begin
               (send lti set-cr-match)
               (send sti set-cr-match)
               'acct-amount-cr-matches)]
            [else false]))))

(define (is-ledger-statement-match-date li si)
  (>= (statement-item-date si) (ledger-item-date li)))

(define (categorize-ledger-statement-match-amount acctid li si)
  (let ([acct-match (compare-acct-dr-cr acctid li si)]
        [li-signed-amount (ledger-signed-amount acctid li)]
        [si-amount (statement-item-amount si)])
    (if (= li-signed-amount si-amount)
        (cond [(symbol=? acct-match 'acct-dr-and-cr-match) 'acct-amount-dr-and-cr-match]
              [(symbol=? acct-match 'acct-dr-matches)      'acct-amount-dr-matches]
              [(symbol=? acct-match 'acct-cr-matches)      'acct-amount-cr-matches]
              [else                                        false])
        false)))

(define (compare-acct-dr-cr acctid li si)
  (let ([mask (bitwise-ior (if (string=? acctid (statement-item-acctid si)) 1 0)
                           (if (string=? acctid (ledger-item-dr-acctid li)) 2 0)
                           (if (string=? acctid (ledger-item-cr-acctid li)) 4 0))])
    (cond [(= mask 7) 'acct-dr-and-cr-match]
          [(= mask 3) 'acct-dr-matches]
          [(= mask 5) 'acct-cr-matches]
          [else false])))

(define (expected-ledger-tag ymd8-statement a-ledger-item)
  (let* ([statement-year (quotient ymd8-statement 10000)]
         [statement-month (quotient (remainder ymd8-statement 10000) 100)]
         [ledger-item-year (quotient (ledger-item-date a-ledger-item) 10000)])
    (fmt-i-02d (+ statement-month (* 12 (- statement-year ledger-item-year))))))

(define (get-ledger-tag acctid a-ledger-item acct-amount-match)
  (cond [(symbol=? acct-amount-match 'acct-amount-dr-matches) (ledger-item-dr-seen a-ledger-item)]
        [(symbol=? acct-amount-match 'acct-amount-cr-matches) (ledger-item-cr-seen a-ledger-item)]
        [(symbol=? acct-amount-match 'acct-amount-dr-and-cr-match)
         (appropriate-ledger-item-tag acctid a-ledger-item)]
        [else false]))

(define (does-ledger-tag-match-statement acctid ymd8-statement a-ledger-item acct-amount-match)
  (let ([actual-ledger-tag (get-ledger-tag acctid a-ledger-item acct-amount-match)])
    (and (string? actual-ledger-tag)
         (string=? actual-ledger-tag (expected-ledger-tag ymd8-statement a-ledger-item)))))

(define (is-special-ledger-tag-match acctid a-ledger-item acct-amount-match)
  (let ([actual-ledger-tag (get-ledger-tag acctid a-ledger-item acct-amount-match)])
    (and (string? actual-ledger-tag)
         (or (string=? actual-ledger-tag ":")
             (member actual-ledger-tag std-skip-tags)))))

(define (appropriate-ledger-item-tag acctid a-ledger-item)
  (let ([signed-amount (ledger-signed-amount acctid a-ledger-item)])
    (cond [(< signed-amount 0) (ledger-item-cr-seen a-ledger-item)]
          [(> signed-amount 0) (ledger-item-dr-seen a-ledger-item)]
          [else (pick-dr-or-cr-tag acctid a-ledger-item)])))

(define (pick-dr-or-cr-tag acctid a-ledger-item)
  (cond [(string=? acctid (ledger-item-dr-acctid a-ledger-item)) (ledger-item-dr-seen a-ledger-item)]
        [(string=? acctid (ledger-item-cr-acctid a-ledger-item)) (ledger-item-cr-seen a-ledger-item)]
        [else false]))

(define std-skip-tags (list "bf" "v"))

(define (sum-ledger-acct-items acctid ledger-items)
  (foldl + 0 (map (λ (li) (ledger-signed-amount acctid li)) ledger-items)))

(define (format-ledger-items acctid ledger-items)
  (apply string-append (map (λ (li) (format-ledger-item acctid li)) ledger-items)))

(define (fpr-ledger-items acctid ledger-items port)
  (fprintf port (format-ledger-items acctid ledger-items)))

(define (pr-ledger-items acctid ledger-items)
  (fprintf (current-output-port) (format-ledger-items acctid ledger-items)))

(define (pr-ledger-items-and-sum acctid ledger-items)
  (pr-ledger-items acctid ledger-items)
  (printf "TOTAL:   ~a\n"
          (~a (format-exact (sum-ledger-acct-items acctid ledger-items) 2)
              #:min-width 9 #:align 'right)))

;; TIP: great for finding which transactions on ledger cleared after statement date
(define (pr-stmt-unmatched-ledger-items acctid ymd8-end)
  (let ([lis (filtered-stmt-unmatched-ledger-items acctid ymd8-end)])
    (pr-ledger-items acctid lis)
    (printf "TOTAL:   ~a\n"
            (~a (format-exact (sum-ledger-acct-items acctid lis) 2)
                #:min-width 9 #:align 'right))))

(define (pr-cleared-after-stmt-ledger-items acctid ymd8-end)
  (let ([lis (filtered-cleared-after-stmt-ledger-items acctid ymd8-end)])
    (pr-ledger-items acctid lis)
    (printf "TOTAL:   ~a\n"
            (~a (format-exact (sum-ledger-acct-items acctid lis) 2)
                #:min-width 9 #:align 'right))))

(define (sum-outstanding-ledger-items acctid ymd8-end)
  (sum-ledger-acct-items acctid (current-outstanding-ledger-items acctid ymd8-end)))

(define (format-outstanding-ledger-items acctid ymd8-end)
  (let ([lis (current-outstanding-ledger-items acctid ymd8-end)])
    (format "~a~a~a\n"
            (format-ledger-items acctid lis)
            "TOTAL:   "
            (~a (format-exact (sum-ledger-acct-items acctid lis) 2)
                #:min-width 9 #:align 'right))))

(define (format-stmt-unmatched-ledger-items acctid ymd8-end)
  (let ([lis (filtered-stmt-unmatched-ledger-items acctid ymd8-end)])
    (format "~a~a~a\n"
            (format-ledger-items acctid lis)
            "TOTAL:   "
            (~a (format-exact (sum-ledger-acct-items acctid lis) 2)
                #:min-width 9 #:align 'right))))

(define (fpr-outstanding-ledger-items acctid ymd8-end port)
  (fprintf port (format-outstanding-ledger-items acctid ymd8-end)))

(define (pr-outstanding-ledger-items acctid ymd8-end)
  (fpr-outstanding-ledger-items acctid ymd8-end (current-output-port)))

(define (current-outstanding-ledger-items acctid ymd8-end)
  (ledger-items-outstanding acctid ymd8-end all-ledger-items))

(define (filtered-stmt-unmatched-ledger-items acctid ymd8-end)
  (let* ([a (unmatched-ledger-items-to-date acctid ymd8-end)]
         [b (ledger-items-exclude-tags std-skip-tags acctid a)])
    (ledger-items-exclude-prior-matches acctid ymd8-end b)))

(define (filtered-cleared-after-stmt-ledger-items acctid ymd8-end)
  (let ([lis (unmatched-ledger-items-to-date acctid ymd8-end)])
    (ledger-items-cleared-after-stmt acctid ymd8-end lis)))

(define (ledger-items-exclude-prior-matches acctid ymd8-end ledger-items)
  (filter (λ (a-ledger-item)
            (not (is-prior-month-ledger-item-match acctid ymd8-end a-ledger-item)))
          ledger-items))

(define (ledger-items-cleared-after-stmt acctid ymd8-end ledger-items)
  (filter (λ (a-ledger-item)
            (is-cleared-after-stmt acctid ymd8-end a-ledger-item))
          ledger-items))

(define (ledger-items-outstanding acctid ymd8-end ledger-items)
  (filter (λ (li)
            (and (or (string=? acctid (ledger-item-dr-acctid li))
                     (string=? acctid (ledger-item-cr-acctid li)))
                 (is-ledger-outstanding-item-as-of acctid ymd8-end li)))
          ledger-items))

(define (is-prior-month-ledger-item-match acctid ymd8-end a-ledger-item)
  (is-prior/later-month-ledger-item-match acctid ymd8-end a-ledger-item <))

(define (is-later-month-ledger-item-match acctid ymd8-end a-ledger-item)
  (is-prior/later-month-ledger-item-match acctid ymd8-end a-ledger-item >))

(define (is-prior/later-month-ledger-item-match acctid ymd8-end a-ledger-item op)
  (let ([yyyymm-current (year-month ymd8-end)])                         ; eg, 201602
    (let* ([yyyymmdd-li (ledger-item-date a-ledger-item)]               ; eg, 20160115
           [yyyymm-li (year-month (ledger-item-date a-ledger-item))]    ; eg, 201601
           [tag (appropriate-ledger-item-tag acctid a-ledger-item)]) ; eg, "01"
      (if (and (string? tag) (number? (string->number tag)))
          (op (effective-year-month yyyymmdd-li (string->number tag)) yyyymm-current)
          false))))

(define (stmt-outstanding-item-amount-as-of ymd8-end typ acctid a-ledger-item)
  (let ([x-amount (ledger-item-amount a-ledger-item)]
        [x-dr-seen (ledger-item-dr-seen a-ledger-item)]
        [x-cr-seen (ledger-item-cr-seen a-ledger-item)])
    (cond [(symbol=? typ 'dr)
           (if (and (string=? acctid (ledger-item-dr-acctid a-ledger-item))
                    (is-stmt-outstanding-item-as-of acctid ymd8-end a-ledger-item x-dr-seen))
               x-amount
               0)]
          [(symbol=? typ 'cr)
           (if (and (string=? acctid (ledger-item-cr-acctid a-ledger-item))
                    (is-stmt-outstanding-item-as-of acctid ymd8-end a-ledger-item x-cr-seen))
               (- x-amount)
               0)]
          [else (error "outstanding-as-of: typ neither 'dr nor 'cr, instead got: " typ)])))

; matches #f, : and later-month items
(define (is-stmt-outstanding-item-as-of acctid ymd8-end a-ledger-item seen)
  (or (false? seen)
      (and (string? seen) (string=? seen ":"))
      (is-later-month-ledger-item-match acctid ymd8-end a-ledger-item)))

; matches : and later-month items
(define (is-cleared-after-stmt acctid ymd8-end a-ledger-item seen)
  (or (and (string? seen) (string=? seen ":"))
      (is-later-month-ledger-item-match acctid ymd8-end a-ledger-item)))

; matches only #f items
(define (is-ledger-outstanding-item-as-of acctid ymd8-end a-ledger-item)
  (let ([tag (appropriate-ledger-item-tag acctid a-ledger-item)])
    (and (<= (ledger-item-date a-ledger-item) ymd8-end)
         (false? tag))))

(define (tag-means-cleared tag)
  (member tag (list ":" "bf" "v")))
    
(define (ledger-items-exclude-tags tags-to-exclude acctid ledger-items)
  (filter (λ (li)
            (not (member (if (> (ledger-signed-amount acctid li) 0)
                             (ledger-item-dr-seen li)
                             (ledger-item-cr-seen li))
                         tags-to-exclude)))
          ledger-items))

(define (unmatched-ledger-items-to-date acctid ymd8-end)
  (let-values ([(statement-unmatched ledger-unmatched) (examine-acct acctid ymd8-end)])
    (map (λ (x)
           (send x get-item))
         ledger-unmatched)))

(define (pr-examine-acct acctid ymd8-end)
  (let-values ([(statement-unmatched ledger-unmatched) (examine-acct acctid ymd8-end)])
    (printf "====== Statement items not in ledger:\n")
    (for-each (λ (x)
                (let ([si (send x get-item)])
                  (printf "~a ~a ~a\n" (statement-item-date si)
                          (~a (format-exact (statement-item-amount si) 2)
                              #:min-width 9 #:align 'right)
                          (statement-item-description si))))
              statement-unmatched)
    (printf "\n====== Ledger items not in loaded statements:\n")
    (for-each (λ (x)
                (let ([li (send x get-item)])
                  (pr-ledger-item acctid li)))
              ledger-unmatched)))

(define (format-ledger-item acctid li)
  (if (or (string=? acctid (ledger-item-dr-acctid li))
          (string=? acctid (ledger-item-cr-acctid li)))
      (format "~a ~a ~a ~a / ~a\n"
              (ledger-item-date li)
              (~a (format-exact (ledger-signed-amount acctid li) 2)
                  #:min-width 9 #:align 'right)
              (~a (if (> (ledger-signed-amount acctid li) 0)
                      (ledger-item-dr-seen li)
                      (ledger-item-cr-seen li))
                  #:min-width 2 #:align 'left)
              (ledger-item-payee li)
              (ledger-item-description li))
      ""))

(define (fpr-ledger-item acctid li port)
  (let ([s (format-ledger-item acctid li)])
    (when (> (string-length s) 0)
      (fprintf port (format-ledger-item acctid li)))))

(define (pr-ledger-item acctid li)
  (fpr-ledger-item acctid li (current-output-port)))

(define (examine-acct acctid ymd8-end)
  (let* ([statement-acct-items (statement-acct-matches acctid (statement-range jan01 ymd8-end all-statement-items))]
         [ledger-acct-items (ledger-acct-matches acctid (ledger-range jan01 ymd8-end all-ledger-items))]
         [statement-acct-track-items (map (λ (x) (new track-item [item x])) statement-acct-items)]
         [ledger-acct-track-items (map (λ (x) (new track-item [item x])) ledger-acct-items)]
         [stmt-dates (list->vector (filter (λ (x) (and (>= x jan01) (<= x ymd8-end)))
                                           (map first (db-get-statement-balances acctid))))])
    (for/list ([stmt-i (in-range (vector-length stmt-dates))])
      (let ([stmt-ymd8 (vector-ref stmt-dates stmt-i)])
        (for/list ([lti ledger-acct-track-items])
          (for/list ([sti (filter (λ (x)
                                    (<= (statement-item-date (send x get-item)) stmt-ymd8))
                                  statement-acct-track-items)])
            (check-ledger-statement-match stmt-ymd8 acctid lti sti)))))
    (values (filter (λ (sti)
                      (let ([amount (statement-item-amount (send sti get-item))])
                        (cond [(< amount 0) (send sti cr-unmatched?)]
                              [(> amount 0) (send sti dr-unmatched?)]
                              [else         (send sti neither-dr-nor-cr-matched?)])))
                    statement-acct-track-items)
            (filter (λ (lti)
                      (let ([amount (ledger-signed-amount acctid (send lti get-item))])
                        (cond [(< amount 0) (send lti cr-unmatched?)]
                              [(> amount 0) (send lti dr-unmatched?)]
                              [else         (send lti neither-dr-nor-cr-matched?)])))
                    ledger-acct-track-items))))

(define (amounts-new-dr-cr ymd8 typ acctid ledger-items)
  (map (λ (li)
         (stmt-outstanding-item-amount-as-of ymd8 typ acctid li))
       ledger-items))

(define (sum-amounts-new-cr acctid stmt-ymd8 ledger-items)
  (apply + (amounts-new-dr-cr stmt-ymd8 'cr acctid ledger-items)))

(define (sum-amounts-new-dr acctid stmt-ymd8 ledger-items)
  (apply + (amounts-new-dr-cr stmt-ymd8 'dr acctid ledger-items)))

; balance shown on statement
; + deposits in ledger that are not on statement
; - outstanding checks/withdrawals not shown on statement
; total should be same as statement

(define (pr-unreconciled acctid ymd8)
  (let* ([reconciliation-ledger-items (db-get-reconciliation-ledger-items acctid ymd8 (db-get-statement-balances acctid))])
    (printf "\n======== ~a TOTAL ===== Debits Not Reconciled to a Statement\n"
            (~a (format-exact (sum-amounts-new-dr acctid ymd8 reconciliation-ledger-items) 2)
                #:min-width 9 #:align 'right))
    (for-each (λ (x)
                (when (string=? acctid (ledger-item-dr-acctid x))
                  (printf "~a ~a ~a (~a) ~a / ~a\n"
                          (ledger-item-date x)
                          (~a (format-exact (ledger-item-amount x) 2)
                              #:min-width 9 #:align 'right)
                          (if (ledger-item-dr-seen x) ":" " ")
                          (ledger-item-cr-acctid x)
                          (ledger-item-payee x)
                          (ledger-item-description x))))
              reconciliation-ledger-items)
    (printf "\n======== ~a TOTAL ===== Credits Not Reconciled to a Statement\n"
            (~a (format-exact (sum-amounts-new-cr acctid ymd8 reconciliation-ledger-items) 2)
                #:min-width 9 #:align 'right))
    (for-each (λ (x)
                (when (string=? acctid (ledger-item-cr-acctid x))
                  (printf "~a ~a ~a (~a) ~a / ~a\n"
                          (ledger-item-date x)
                          (~a (format-exact (- (ledger-item-amount x)) 2)
                              #:min-width 9 #:align 'right)
                          (if (ledger-item-cr-seen x) ":" " ")
                          (ledger-item-dr-acctid x)
                          (ledger-item-payee x)
                          (ledger-item-description x))))
              reconciliation-ledger-items)))

; ymd8 -> ym6
(define (year-month ymd8)
  (let ([yyyy (quotient ymd8 10000)]
        [mm (quotient (remainder ymd8 10000) 100)])
    (+ (* 100 yyyy) mm)))

; 2017mmdd 01 -> 201701
; 2016mmdd 13 -> 201701
; 2015mmdd 25 -> 201701
; ymd8 integer -> ym6
(define (effective-year-month ymd8 tag-month)
  (let-values ([(quo rem) (quotient/remainder tag-month 12)])
    (+ (* 100 (+ (quotient ymd8 10000) (* quo))) rem)))

; integer integer -> (oneof -1 0 1)
(define (compare-year-months a b)
  (cond [(< a b) -1]
        [(= a b) 0]
        [(> a b) 1]))
