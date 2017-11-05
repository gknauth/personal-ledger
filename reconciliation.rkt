#lang racket

(require (prefix-in s19: (lib "19.ss" "srfi"))
         (prefix-in s48: (lib "48.ss" "srfi"))
         db
         plot
         racket/date
         data/gvector
         format-numbers
         format-ymd
         (file "~/.ledger-dbaccess.rkt")
         (file "~/.ledger-prefs.rkt"))

(provide (all-defined-out))

(define jan01 (+ (* 10000 start-year) 101))
(define dec31 (+ (* 10000 end-year) 1231))

(struct ledger-item (date amount dr-acct cr-acct payee description dr-seen cr-seen dr-deduct cr-deduct) #:transparent)
(struct statement-item (acct date amount description) #:transparent)

(struct ledger-bal-item (ledger-item balance balance-seen diff) #:transparent)

(struct reconciliation-item (date amount dr-acct cr-acct payee description dr-seen cr-seen) #:transparent)

(define con (mysql-connect #:server db-host
                           #:database db-schema
                           #:user db-user
                           #:password db-passwd
                           #:port db-port))

(define today (today->ymd8))

(define (sub-false-for-sql-null x)
  (if (sql-null? x) #f x))

(define (row-to-statement-item x)
  (statement-item (vector-ref x 1) (sql-date->ymd8 (vector-ref x 2)) (vector-ref x 3) (vector-ref x 4)))

(define (row-to-ledger-item x)
  (ledger-item (sql-date->ymd8 (vector-ref x 1)) (vector-ref x 2) (vector-ref x 3) (vector-ref x 4) (vector-ref x 5) (vector-ref x 6)
               (sub-false-for-sql-null (vector-ref x 7))
               (sub-false-for-sql-null (vector-ref x 8))
               (sub-false-for-sql-null (vector-ref x 9))
               (sub-false-for-sql-null (vector-ref x 10))))

(define (row-to-reconciliation-item x)
  (reconciliation-item (sql-date->ymd8 (vector-ref x 0)) (vector-ref x 1) (vector-ref x 2) (vector-ref x 3)
                       (sub-false-for-sql-null (vector-ref x 4))
                       (sub-false-for-sql-null (vector-ref x 5))
                       (sub-false-for-sql-null (vector-ref x 6))
                       (sub-false-for-sql-null (vector-ref x 7))))

; string -> (list-of (list number number))
(define (get-statement-balances acct)
  (let* ([accttype (substring acct 0 1)]
         [acctname (substring acct 2)]
         [rows (query-rows
                con
                (string-append "select date, amount from balances where acct_type = '" accttype
                               "' and acct_name = '" acctname
                               "' and isstmt is true order by date"))])
    (map (λ (row)
           (list (sql-date->ymd8 (vector-ref row 0)) (vector-ref row 1))) rows)))

; num num -> (list-of statement-item)
(define (get-statement-items start-year end-year)
  (let ([rows (query-rows
               con (string-append "select * from statements where date>='" (number->string start-year) "-01-01' and date<='" (number->string end-year) "-12-31' order by date"))])
    (map row-to-statement-item rows)))

(define (find-previous-statement-date ymd8 statement-balances-going-back-in-time)
  (cond [(empty? statement-balances-going-back-in-time) jan01]  ;; FIXME will eventually filter from jan02 not jan01
        [else (let ([x (first (first statement-balances-going-back-in-time))])
                (if (< x ymd8)
                    x
                    (find-previous-statement-date ymd8 (rest statement-balances-going-back-in-time))))]))

(define (find-reconciliation-items acct ymd8-end statement-balances)
  (let* ([ymd8-start (find-previous-statement-date ymd8-end (reverse statement-balances))]
         [s-month (substring (ymd8->ymd10 ymd8-end) 5 7)]
         [rows (query-rows
                con (string-append "select"
                                   " date,amount,dr_acct,cr_acct,payee,description,dr_seen,cr_seen"
                                   " from ledger where"
                                   " (date >  '" (ymd8->ymd10 ymd8-start) "' and"
                                   "  date <= '" (ymd8->ymd10 ymd8-end)   "')"
                                   " and"
                                   " ((dr_acct = '" acct "' and (dr_seen is null or dr_seen != '" s-month "' )) "
                                   "   or "
                                   "  (cr_acct = '" acct "' and (cr_seen is null or cr_seen != '" s-month "' )) )"
                                   " order by date")
                )])
    (map row-to-reconciliation-item rows)))

(define (first-ledger-unseen-discrepancy acct)
  (let ([lbis (ledger-unseen-discrepancies acct)])
    (if (empty? lbis)
        #f
        (let ([lbi (first lbis)])
          (list (- (ledger-bal-item-balance-seen lbi) (ledger-bal-item-balance lbi)) lbi)))))

(define (ledger-unseen-discrepancies acct)
  (filter (λ (x)
            (not (= (ledger-bal-item-balance x)
                    (ledger-bal-item-balance-seen x))))
          (get-ledger-bal-items acct)))

(define (ledger-unseen-discrepancy-changes acct)
  (define (helper acct ins outs running-discrepancy)
    (if (empty? ins)
        outs
        (let ([new-diff (ledger-bal-item-diff (first ins))])
          (if (= new-diff running-discrepancy)
              (helper acct (rest ins) outs running-discrepancy)
              (helper acct (rest ins) (cons (first ins) outs) new-diff)))))
  (reverse (helper acct (ledger-unseen-discrepancies acct) empty 0)))

; num num -> (list-of ledger-item)
(define (get-ledger-items start-year end-year)
  (let ([rows (query-rows
               con (string-append "select * from ledger where date>='" (number->string start-year) "-01-01' and date<='" (number->string end-year) "-12-31' order by date"))])
    (map row-to-ledger-item rows)))

(define (statement-filter-acct acct statement-items)
  (filter (λ (srow) (string=? (statement-item-acct srow) acct)) statement-items))

(define (ledger-filter-acct acct ledger-items)
  (filter (λ (lrow)
            (or (string=? (ledger-item-dr-acct lrow) acct)
                (string=? (ledger-item-cr-acct lrow) acct)))
          ledger-items))

(define (statement<=ymd8 ymd8 statement-items)
  (filter (λ (srow) (<= (statement-item-date srow) ymd8)) statement-items))

(define (statement>=ymd8 ymd8 statement-items)
  (filter (λ (srow) (>= (statement-item-date srow) ymd8)) statement-items))

(define (statement-range ymd8-a ymd8-b statement-items)
  (filter (λ (srow)
            (and (>= (statement-item-date srow) ymd8-a) (<= (statement-item-date srow) ymd8-b)))
          statement-items))

(define (ledger<=ymd8 ymd8 lst)
  (filter (λ (lrow) (<= (ledger-item-date lrow) ymd8)) lst))

(define (ledger>=ymd8 ymd8 lst)
  (filter (λ (lrow) (>= (ledger-item-date lrow) ymd8)) lst))

(define (ledger-range ymd8-a ymd8-b lst)
  (filter (λ (lrow)
            (and (>= (ledger-item-date lrow) ymd8-a) (<= (ledger-item-date lrow) ymd8-b)))
          lst))

(define (ledger-range-acct acct ymd8-a ymd8-b lst)
  (filter (λ (lrow)
            (or (string=? acct (ledger-item-dr-acct lrow))
                (string=? acct (ledger-item-cr-acct lrow))))
          (ledger-range ymd8-a ymd8-b (ledger-filter-acct acct lst))))

(define (ledger-range-acct-dr acct ymd8-a ymd8-b lst)
  (filter (λ (lrow)
            (string=? acct (ledger-item-dr-acct lrow)))
          (ledger-range ymd8-a ymd8-b (ledger-filter-acct acct lst))))

(define (ledger-range-acct-cr acct ymd8-a ymd8-b lst)
  (filter (λ (lrow)
            (string=? acct (ledger-item-cr-acct lrow)))
          (ledger-range ymd8-a ymd8-b (ledger-filter-acct acct lst))))

(define (ledger-range-amts acct ymd8-a ymd8-b lst)
  (map (λ (lrow)
         (ledger-amt acct lrow))
       (ledger-range ymd8-a ymd8-b (ledger-filter-acct acct lst))))

(define (ledger-range-amts-seen acct ymd8-a ymd8-b lst)
  (map (λ (lrow)
         (ledger-amt-seen acct lrow))
       (ledger-range ymd8-a ymd8-b (ledger-filter-acct acct lst))))

(define (ledger-range-amts-unseen acct ymd8-a ymd8-b lst)
  (map (λ (lrow)
         (ledger-amt-unseen acct lrow))
       (ledger-range ymd8-a ymd8-b (ledger-filter-acct acct lst))))

(define (sum-ledger-range-amts acct ymd8-a ymd8-b lst)
  (foldl + 0 (ledger-range-amts acct ymd8-a ymd8-b lst)))

(define (sum-ledger-range-amts-seen acct ymd8-a ymd8-b lst)
  (foldl + 0 (ledger-range-amts-seen acct ymd8-a ymd8-b lst)))

(define (sum-ledger-range-amts-unseen acct ymd8-a ymd8-b lst)
  (foldl + 0 (ledger-range-amts-unseen acct ymd8-a ymd8-b lst)))

(define (ledger-amt-dr acct a-ledger-item)
  (if (string=? acct (ledger-item-dr-acct a-ledger-item))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amt-cr acct a-ledger-item)
  (if (string=? acct (ledger-item-cr-acct a-ledger-item))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amt-dr-seen acct a-ledger-item)
  (if (and (ledger-item-dr-seen a-ledger-item)
           (string=? acct (ledger-item-dr-acct a-ledger-item)))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amt-cr-seen acct a-ledger-item)
  (if (and (ledger-item-cr-seen a-ledger-item)
           (string=? acct (ledger-item-cr-acct a-ledger-item)))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amt-dr-unseen acct a-ledger-item)
  (if (and (not (ledger-item-dr-seen a-ledger-item))
           (string=? acct (ledger-item-dr-acct a-ledger-item)))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amt-cr-unseen acct lrow)
  (if (and (not (ledger-item-cr-seen lrow))
           (string=? acct (ledger-item-cr-acct lrow)))
      (ledger-item-amount lrow)
      0))

(define (ledger-amt acct a-ledger-item)
  (let ([ltr (string-ref acct 0)])
    (cond [(or (eq? ltr #\a) (eq? ltr #\l) (eq? ltr #\e))
           (- (ledger-amt-dr acct a-ledger-item) (ledger-amt-cr acct a-ledger-item))]
          [(or (eq? ltr #\r) (eq? ltr #\x))
           (- (ledger-amt-cr acct a-ledger-item) (ledger-amt-dr acct a-ledger-item))]
          [else 0])))

(define (ledger-amt-seen acct a-ledger-item)
  (let ([ltr (string-ref acct 0)])
    (cond [(or (eq? ltr #\a) (eq? ltr #\l) (eq? ltr #\e))
           (- (ledger-amt-dr-seen acct a-ledger-item)
              (ledger-amt-cr-seen acct a-ledger-item))]
          [(or (eq? ltr #\r) (eq? ltr #\x))
           (- (ledger-amt-cr-seen acct a-ledger-item)
              (ledger-amt-dr-seen acct a-ledger-item))]
          [else 0])))

(define (ledger-amt-unseen acct a-ledger-item)
  (let ([ltr (string-ref acct 0)])
    (cond [(or (eq? ltr #\a) (eq? ltr #\l) (eq? ltr #\e)) (- (ledger-amt-dr-unseen acct a-ledger-item) (ledger-amt-cr-unseen acct a-ledger-item))]
          [(or (eq? ltr #\r) (eq? ltr #\x))               (- (ledger-amt-cr-unseen acct a-ledger-item) (ledger-amt-dr-unseen acct a-ledger-item))]
          [else 0])))

(define (sql-date->ymd8 d)
  (+ (* (sql-date-year d) 10000)
     (* (sql-date-month d) 100)
     (* (sql-date-day d))))

(define all-statement-items (get-statement-items start-year end-year))
(define all-ledger-items (get-ledger-items start-year end-year))

(define (get-ledger-bal-items acct)
  (get-ledger-bal-items-from acct all-ledger-items 0 0))

(define (get-ledger-bal-items-from acct
                                   ledger-items starting-balance starting-balance-seen)
  (define (helper ins outs running-balance running-balance-seen)
    (if (empty? ins)
        outs
        (let* ([in (first ins)]
               [amt (ledger-amt acct in)]
               [amt-seen (ledger-amt-seen acct in)]
               [newbal (+ running-balance amt)]
               [newbal-seen (+ running-balance-seen amt-seen)])
          (helper (rest ins)
                  (cons (ledger-bal-item in newbal newbal-seen (- newbal newbal-seen)) outs)
                  newbal
                  newbal-seen))))
  (let ([ins (ledger-filter-acct acct ledger-items)])
    (reverse (helper ins empty starting-balance starting-balance-seen))))

(define track-item
  (class object%
    (super-new)
    (field [_matched #f])
    (init-field item)
    (define/public (match)
      (set! _matched #t))
    (define/public (get-item) item)
    (define/public (matched?) _matched)
    (define/public (unmatched?) (not _matched))))

(define (is-ledger-statement-new-match acct ts tl)
  (if (send tl matched?)
      #f
      (let* ([srow (send ts get-item)]
             [lrow (send tl get-item)]
             [l-date (ledger-item-date lrow)]
             [s-date (statement-item-date srow)]
             [l-amt2 (ledger-amt acct lrow)]
             [s-amt2 (statement-item-amount srow)])
        (and (>= s-date l-date)
             (= l-amt2 s-amt2)))))

(define (pr-examine-acct acct)
  (let ([results (examine-acct acct)])
    (printf "====== Statement items not in ledger:\n")
    (for-each (λ (srow)
                (printf "~a ~a ~a\n" (statement-item-date srow)
                        (exact->inexact (statement-item-amount srow))
                        (statement-item-description srow)))
              (first results))
    (printf "\n====== Ledger items not in loaded statements:\n")
    (for-each (λ (lrow)
                (printf "~a ~a ~a ~a ~a\n"
                        (ledger-item-date lrow)
                        (if (> (ledger-amt acct lrow) 0)
                            (ledger-item-dr-seen lrow)
                            (ledger-item-cr-seen lrow))
                        (exact->inexact (ledger-amt acct lrow))
                        (ledger-item-payee lrow)
                        (ledger-item-description lrow)))
              (second results))))

(define (examine-acct acct)
  (let* ([statement-acct-items (statement-filter-acct acct (statement-range jan01 today all-statement-items))]
         [ledger-acct-items (ledger-filter-acct acct (ledger-range jan01 today all-ledger-items))]
         [statement-acct-track-items (map (λ (x) (new track-item [item x])) statement-acct-items)]
         [ledger-acct-track-items (map (λ (x) (new track-item [item x])) ledger-acct-items)]
         [statement-unmatched empty])
    (for/list ([statement-acct-track-item statement-acct-track-items])
      (for/list ([ledger-acct-track-item ledger-acct-track-items])
        (when (is-ledger-statement-new-match acct statement-acct-track-item ledger-acct-track-item)
          (send ledger-acct-track-item match)
          (send statement-acct-track-item match)))
      (when (send statement-acct-track-item unmatched?)
        (set! statement-unmatched (cons (send statement-acct-track-item get-item) statement-unmatched))))
    (list (reverse statement-unmatched)
          (map (λ (x)
                 (send x get-item))
               (filter (λ (x)
                         (send x unmatched?))
                       ledger-acct-track-items)))))

(define (amounts-seen-but-not-in-statement acct ymd8 statement-balances)
  (map (λ (x)
         (let ([x-amount (reconciliation-item-amount x)]
               [x-dr-seen (reconciliation-item-dr-seen x)]
               [x-cr-seen (reconciliation-item-cr-seen x)])
           (+ (if (or (string? x-dr-seen) (symbol? x-dr-seen))
                  x-amount
                  0)
              (if (or (string? x-cr-seen) (symbol? x-cr-seen))
                  (- x-amount)
                  0))))
       (find-reconciliation-items acct ymd8 statement-balances)))

(define (sum-amounts-seen-but-not-in-statement acct ymd8 statement-balances)
  (apply + (amounts-seen-but-not-in-statement acct ymd8 statement-balances)))
