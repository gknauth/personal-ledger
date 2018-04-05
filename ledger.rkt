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

(struct ledger-item (date amount dr-acct cr-acct payee description dr-seen cr-seen dr-deduct cr-deduct) #:transparent)
(struct statement-item (acct date amount description) #:transparent)

(struct ledger-bal-item (ledger-item balance balance-seen diff) #:transparent)

(struct day-bal (date balance) #:transparent)

(define track-item
  (class object%
    (super-new)
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

(define con (mysql-connect #:server db-host
                           #:database db-schema
                           #:user db-user
                           #:password db-passwd
                           #:port db-port))

(define all-stmt-bals (make-hash))

;;; Database Calls

; string -> (list-of (list number number))
(define (db-get-statement-balances acct)
  (let* ([accttype (substring acct 0 1)]
         [acctname (substring acct 2)]
         [rows (query-rows
                con
                (string-append "select date, amount from balances where acct_type = '" accttype
                               "' and acct_name = '" acctname
                               "' and isstmt is true order by date"))])
    (map (λ (row)
           (list (sql-date->ymd8 (vector-ref row 0)) (vector-ref row 1))) rows)))

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
               con (string-append "select * from statements where date>='" (number->string start-year) "-01-01' and date<='" (number->string end-year) "-12-31' order by date"))])
    (map row-to-statement-item rows)))

(define (db-get-reconciliation-ledger-items acct ymd8-end statement-balances)
  (let* ([ymd8-start (find-previous-statement-date ymd8-end (reverse statement-balances))]
         [rows (query-rows
                con (string-append
                     "select"
                     " id,date,amount,dr_acct,cr_acct,payee,description,dr_seen,cr_seen,dr_deduct,cr_deduct"
                     " from ledger where"
                     " (date >  '" (ymd8->ymd10 ymd8-start) "' and"
                     "  date <= '" (ymd8->ymd10 ymd8-end)   "')"
                     " and"
                     " ((dr_acct = '" acct "' and (dr_seen is null or dr_seen = ':' )) "
                     "   or "
                     "  (cr_acct = '" acct "' and (cr_seen is null or cr_seen = ':' )) )"
                     " order by date"))])
    (map row-to-ledger-item rows)))

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
               con (string-append "select id,date,amount,dr_acct,cr_acct,payee,description,dr_seen,cr_seen,dr_deduct,cr_deduct from ledger where date>='" (number->string start-year) "-01-01' and date<='" (number->string end-year) "-12-31' order by date"))])
    (map row-to-ledger-item rows)))

(define all-statement-items (db-get-statement-items start-year end-year))
(define all-ledger-items (db-get-ledger-items start-year end-year))

;;; End Database Calls

(define (get-book-ext-diff acct ymd8)
  (let* ([xs (get-ledger-bal-items acct)]
         [bals (acct-book-and-ext-balances-on-date acct ymd8)]
         [book (first bals)]
         [ext (second bals)]
         [ext-minus-book (- ext book)])
    (list book ext ext-minus-book)))

(define (acct-book-and-ext-balances-on-date acct as-of)
  (let* ([xs (get-ledger-bal-items acct)]
         [ys (filter (λ (x)
                       (<= (ledger-item-date (ledger-bal-item-ledger-item x)) as-of))
                     xs)])
    (if (empty? ys)
        (list 0 0)
        (list (ledger-bal-item-balance (first (reverse ys)))
              (ledger-bal-item-balance-seen (first (reverse ys)))))
    ))

; to return a number ymd8 must be one of the ymd8 values in the list
; (listof (list ymd8 exact)) ymd8 -> exact
(define (get-stmt-bal-for-date date-bals ymd8)
  (cond [(null? date-bals) (error "balance unavailable for date" ymd8)]
        [(= (first (first date-bals)) ymd8) (second (first date-bals))]
        [else (get-stmt-bal-for-date (rest date-bals) ymd8)]))

; string -> (list-of number)
(define (get-statement-dates acct)
  (map first (db-get-statement-balances acct)))

; string number number -> (listof number number number)
(define (get-new-dr-new-cr-reconciliation acct stmt-ymd8)
  (let* ([statement-balances (db-get-statement-balances acct)]
         [stmt-bal (get-stmt-bal-for-date statement-balances stmt-ymd8)]
         [reconciliation-items (db-get-reconciliation-ledger-items acct stmt-ymd8 statement-balances)]
         [new-dr (sum-amounts-new-dr acct stmt-ymd8 reconciliation-items)]
         [new-cr (sum-amounts-new-cr acct stmt-ymd8 reconciliation-items)]
         [reconciliation (+ stmt-bal new-dr new-cr)])
    (list new-dr new-cr reconciliation)))

; string number number -> (listof number number number number)
(define (get-new-dr-new-cr-reconciliation-diff acct stmt-ymd8 amt)
  (let* ([dr-cr-rec (get-new-dr-new-cr-reconciliation acct stmt-ymd8)]
         [new-dr (first dr-cr-rec)]
         [new-cr (second dr-cr-rec)]
         [reconciliation (third dr-cr-rec)]
         [amt-minus-reconciliation (- amt reconciliation)])
    (list new-dr new-cr reconciliation amt-minus-reconciliation)))

; string -> (listof (list number number number number number))
(define (get-reconciliations acct)
  (map (λ (ymd8-bal)
         (cons (first ymd8-bal) (get-new-dr-new-cr-reconciliation-diff acct (first ymd8-bal) (second ymd8-bal))))
       (db-get-statement-balances acct)))

(define (find-previous-statement-date ymd8 statement-balances-going-back-in-time)
  (cond [(empty? statement-balances-going-back-in-time) jan01] ;FIXME needs to be > dec31 not jan01
        [else (let ([x (first (first statement-balances-going-back-in-time))])
                (if (< x ymd8)
                    x
                    (find-previous-statement-date ymd8 (rest statement-balances-going-back-in-time))))]))

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

(define (statement-filter-acct acct statement-items)
  (filter (λ (srow) (string=? (statement-item-acct srow) acct)) statement-items))

(define (ledger-filter-acct acct ledger-items)
  (filter (λ (lrow)
            (or (string=? (ledger-item-dr-acct lrow) acct)
                (string=? (ledger-item-cr-acct lrow) acct)))
          ledger-items))

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

(define (ledger-range-acct acct ymd8-a ymd8-b ledger-items)
  (filter (λ (li)
            (or (string=? acct (ledger-item-dr-acct li))
                (string=? acct (ledger-item-cr-acct li))))
          (ledger-range ymd8-a ymd8-b (ledger-filter-acct acct ledger-items))))

(define (ledger-range-acct-dr acct ymd8-a ymd8-b ledger-items)
  (filter (λ (li)
            (string=? acct (ledger-item-dr-acct li)))
          (ledger-range ymd8-a ymd8-b (ledger-filter-acct acct ledger-items))))

(define (ledger-range-acct-cr acct ymd8-a ymd8-b ledger-items)
  (filter (λ (li)
            (string=? acct (ledger-item-cr-acct li)))
          (ledger-range ymd8-a ymd8-b (ledger-filter-acct acct ledger-items))))

(define (ledger-range-signed-amounts acct ymd8-a ymd8-b ledger-items)
  (map (λ (li)
         (ledger-signed-amount acct li))
       (ledger-range ymd8-a ymd8-b (ledger-filter-acct acct ledger-items))))

(define (ledger-range-signed-amounts-seen acct ymd8-a ymd8-b ledger-items)
  (map (λ (li)
         (ledger-signed-amount-seen acct li))
       (ledger-range ymd8-a ymd8-b (ledger-filter-acct acct ledger-items))))

(define (ledger-range-signed-amounts-unseen acct ymd8-a ymd8-b ledger-items)
  (map (λ (li)
         (ledger-signed-amount-unseen acct li))
       (ledger-range ymd8-a ymd8-b (ledger-filter-acct acct ledger-items))))

(define (sum-ledger-range-signed-amounts acct ymd8-a ymd8-b ledger-items)
  (foldl + 0 (ledger-range-signed-amounts acct ymd8-a ymd8-b ledger-items)))

(define (sum-ledger-range-signed-amounts-seen acct ymd8-a ymd8-b ledger-items)
  (foldl + 0 (ledger-range-signed-amounts-seen acct ymd8-a ymd8-b ledger-items)))

(define (sum-ledger-range-signed-amounts-unseen acct ymd8-a ymd8-b ledger-items)
  (foldl + 0 (ledger-range-signed-amounts-unseen acct ymd8-a ymd8-b ledger-items)))

(define (ledger-amount-dr acct a-ledger-item)
  (if (string=? acct (ledger-item-dr-acct a-ledger-item))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amount-cr acct a-ledger-item)
  (if (string=? acct (ledger-item-cr-acct a-ledger-item))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amount-dr-seen acct a-ledger-item)
  (if (and (ledger-item-dr-seen a-ledger-item)
           (string=? acct (ledger-item-dr-acct a-ledger-item)))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amount-cr-seen acct a-ledger-item)
  (if (and (ledger-item-cr-seen a-ledger-item)
           (string=? acct (ledger-item-cr-acct a-ledger-item)))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amount-dr-unseen acct a-ledger-item)
  (if (and (not (ledger-item-dr-seen a-ledger-item))
           (string=? acct (ledger-item-dr-acct a-ledger-item)))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-amount-cr-unseen acct a-ledger-item)
  (if (and (not (ledger-item-cr-seen a-ledger-item))
           (string=? acct (ledger-item-cr-acct a-ledger-item)))
      (ledger-item-amount a-ledger-item)
      0))

(define (ledger-signed-amount acct a-ledger-item)
  (let ([d (account-division acct)])
    (cond [(eq? d 'balance-sheet)
           (- (ledger-amount-dr acct a-ledger-item) (ledger-amount-cr acct a-ledger-item))]
          [(eq? d 'income-statement)
           (- (ledger-amount-cr acct a-ledger-item) (ledger-amount-dr acct a-ledger-item))]
          [else 0])))

(define (account-division acct)
  (let ([ltr (string-ref acct 0)])
    (cond [(or (eq? ltr #\a) (eq? ltr #\l) (eq? ltr #\e)) 'balance-sheet]
          [(or (eq? ltr #\r) (eq? ltr #\x))               'income-statement]
          [else #f])))

(define (ledger-signed-amount-seen acct a-ledger-item)
  (let ([ltr (string-ref acct 0)])
    (cond [(or (eq? ltr #\a) (eq? ltr #\l) (eq? ltr #\e))
           (- (ledger-amount-dr-seen acct a-ledger-item)
              (ledger-amount-cr-seen acct a-ledger-item))]
          [(or (eq? ltr #\r) (eq? ltr #\x))
           (- (ledger-amount-cr-seen acct a-ledger-item)
              (ledger-amount-dr-seen acct a-ledger-item))]
          [else 0])))

(define (ledger-signed-amount-unseen acct a-ledger-item)
  (let ([ltr (string-ref acct 0)])
    (cond [(or (eq? ltr #\a) (eq? ltr #\l) (eq? ltr #\e))
           (- (ledger-amount-dr-unseen acct a-ledger-item) (ledger-amount-cr-unseen acct a-ledger-item))]
          [(or (eq? ltr #\r) (eq? ltr #\x))
           (- (ledger-amount-cr-unseen acct a-ledger-item) (ledger-amount-dr-unseen acct a-ledger-item))]
          [else 0])))

; string -> (listof ledger-bal-item)
(define (get-ledger-bal-items acct)
  (get-ledger-bal-items-from acct all-ledger-items 0 0))

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

(define (get-day-bals acct)
  (closing-bal-each-day
   (map (λ (lbi)
          (let ([li (ledger-bal-item-ledger-item lbi)])
            (day-bal (ledger-item-date li) (ledger-bal-item-balance lbi))))
        (get-ledger-bal-items acct))))

(define (get-day-bals-filter acct filter-func)
  (map (λ (lbi)
         (let ([li (ledger-bal-item-ledger-item lbi)])
           (day-bal (ledger-item-date li) (ledger-bal-item-balance lbi))))
       (filter filter-func (get-ledger-bal-items acct))))

(define (day-bals-from acct start-ymd8)
  (define filter-func
    (λ (lbi)
      (>= (ledger-item-date (ledger-bal-item-ledger-item lbi)) start-ymd8)))
  (get-day-bals-filter acct filter-func))

(define (day-bals-range acct start-ymd8 end-ymd8)
  (define filter-func
    (λ (lbi)
      (and (>= (ledger-item-date (ledger-bal-item-ledger-item lbi)) start-ymd8)
           (<= (ledger-item-date (ledger-bal-item-ledger-item lbi)) end-ymd8))))
  (get-day-bals-filter acct filter-func))

(define (plot-day-bals-from acct start-ymd8)
  (plot-day-bals acct (day-bals-from acct start-ymd8)))

(define (plot-day-bals-range acct start-ymd8 end-ymd8)
  (plot-day-bals acct (day-bals-range acct start-ymd8 end-ymd8)))

(define (plot-day-bals-forward acct ndays)
  (plot-day-bals-range acct (today->ymd8) (ymd8-plusdays->ymd8 (today->ymd8) ndays)))

(define (plot-accts-day-bals-forward accts ndays)
  2)

(define (plot-day-bals acct bals)
  (let* ([xs (map (λ (b) (date->seconds (ymd8->date (day-bal-date b)))) bals)]
         [ys (map day-bal-balance bals)])
    (parameterize ([plot-title acct]
                   [plot-x-label "Date"]
                   [plot-x-ticks (date-ticks)]
                   [plot-y-label "Amount"])
      (plot (lines (map vector xs ys) #:color (acct-color acct))))))

(define (foo-plot ndays)
  (plot-accts-day-bals-range (list "a chk" "a pnc")
                             (today->ymd8)
                             (ymd8-plusdays->ymd8 (today->ymd8) ndays)))

;(define (ff)
;  (let ([accts (vector "a chk" "a pnc")])
;    (for/vector ([i (in-range (vector-length accts))])
;      (day-bals-range (first accts) start-ymd8 end-ymd8)]
               

(define (plot-accts-day-bals-range accts start-ymd8 end-ymd8)
  (let* ([bals1 (day-bals-range (first accts) start-ymd8 end-ymd8)]
         [bals2 (day-bals-range (second accts) start-ymd8 end-ymd8)]
         [xs1 (map (λ (b) (date->seconds (ymd8->date (day-bal-date b)))) bals1)]
         [ys1 (map day-bal-balance bals1)]
         [xs2 (map (λ (b) (date->seconds (ymd8->date (day-bal-date b)))) bals2)]
         [ys2 (map day-bal-balance bals2)]
         )
        (parameterize (
                   [plot-x-label "Date"]
                   [plot-x-ticks (date-ticks)]
                   [plot-y-label "Amount"])
    (plot (list
          (lines (map vector xs1 ys1))
          (lines (map vector xs2 ys2)))))))


(define (acct-color acct)
  (let ([ch (string-ref acct 0)])
    (cond [(eq? ch #\a) "Forest Green"]
          [(eq? ch #\l) "red"]
          [else "blue"])))

; find first ledger-bal-item where book and ext differ
; string -> (or/c ledger-bal-item #f)
(define (find-first-ledger-bal-item-book-ext-difference acct)
  (let* ([items (get-ledger-bal-items acct)]
         [nz-diffs (filter (λ (x) (not (zero? (ledger-bal-item-diff x)))) items)])
    (if (> (length nz-diffs) 0)
        (first nz-diffs)
        #f)))

(define (get-ledger-bal-items-from
         acct ledger-items starting-balance starting-balance-seen)
  (define (helper ins outs running-balance running-balance-seen)
    (if (empty? ins)
        outs
        (let* ([in (first ins)]
               [signed-amount (ledger-signed-amount acct in)]
               [signed-amount-seen (ledger-signed-amount-seen acct in)]
               [newbal (+ running-balance signed-amount)]
               [newbal-seen (+ running-balance-seen signed-amount-seen)])
          (helper (rest ins)
                  (cons (ledger-bal-item in newbal newbal-seen (- newbal newbal-seen)) outs)
                  newbal
                  newbal-seen))))
  (let ([ins (ledger-filter-acct acct ledger-items)])
    (reverse (helper ins empty starting-balance starting-balance-seen))))

;; L1: 20171030 3.00 (a pnc :) (l lycoming) ("whatever"))
;; L2: 20171031 200.00 (a pnc 11) (l lycoming) ("whatever"))
;; L3: 20171110 5.00 (x food) (a pnc 11) ("whatever"))
;; L4: 20171115 100.00 (a pnc 11) (l lycoming) ("whatever"))
;;
;; S1: 20171101 200.00 "whatever"
;; S2: 20171101 -5.00 "whatever"
;; S3: 20171116 100.00 "whatever"
;;
;; L2 matches S1 because:
;;   Sdate >= Ldate
;;   Ldr-acct match and signed(Lamt) == Samt
;;   Smonth==11 and num(Ldr-tag)==(+ 11 (* 12 (- statement-year ledger-year)))

(define (check-ledger-statement-match ymd8-statement acct tl ts)
  (let* ([lrow (send tl get-item)]
         [srow (send ts get-item)]
         [l-date (ledger-item-date lrow)]
         [s-date (statement-item-date srow)]
         [l-signed-amount (ledger-signed-amount acct lrow)]
         [s-amount (statement-item-amount srow)]
         [statement-month (quotient (remainder ymd8-statement 10000) 100)]
         [acct-amount-match (categorize-ledger-statement-match-amount acct lrow srow)])
    (when (and acct-amount-match
               (is-ledger-statement-match-date lrow srow)
               (does-ledger-tag-match-statement acct ymd8-statement lrow acct-amount-match))
      (cond [(symbol=? acct-amount-match 'acct-amount-dr-and-cr-match)
             (begin
               (send tl set-dr-match)
               (send tl set-cr-match)
               (send ts set-dr-match)
               (send ts set-dc-match))]
            [(symbol=? acct-amount-match 'acct-amount-dr-matches)
             (begin
               (send tl set-dr-match)
               (send ts set-dr-match))]
            [(symbol=? acct-amount-match 'acct-amount-cr-matches)
             (begin
               (send tl set-cr-match)
               (send ts set-cr-match))]
            [else false]))))

(define (is-ledger-statement-match-date li si)
  (>= (statement-item-date si) (ledger-item-date li)))

(define (categorize-ledger-statement-match-amount acct li si)
  (let ([acct-match (compare-acct-dr-cr acct li si)]
        [li-signed-amount (ledger-signed-amount acct li)]
        [si-amount (statement-item-amount si)])
    (if (= li-signed-amount si-amount)
        (cond [(symbol=? acct-match 'acct-dr-and-cr-match) 'acct-amount-dr-and-cr-match]
              [(symbol=? acct-match 'acct-dr-matches)      'acct-amount-dr-matches]
              [(symbol=? acct-match 'acct-cr-matches)      'acct-amount-cr-matches]
              [else                                        false])
        false)))

(define (compare-acct-dr-cr acct li si)
  (let ([mask (bitwise-ior (if (string=? acct (statement-item-acct si)) 1 0)
                           (if (string=? acct (ledger-item-dr-acct li)) 2 0)
                           (if (string=? acct (ledger-item-cr-acct li)) 4 0))])
    (cond [(= mask 7) 'acct-dr-and-cr-match]
          [(= mask 3) 'acct-dr-matches]
          [(= mask 5) 'acct-cr-matches]
          [else false])))

; eg: Smonth==11 and num(Ldr-tag)==(+ 11 (* 12 (- statement-year ledger-year)))
(define (does-ledger-tag-match-statement acct ymd8-statement a-ledger-item acct-amount-match)
  (let* ([statement-year (quotient ymd8-statement 10000)]
         [statement-month (quotient (remainder ymd8-statement 10000) 100)]
         [ledger-item-year (quotient (ledger-item-date a-ledger-item) 10000)]
         [ledger-item-month (quotient (remainder (ledger-item-date a-ledger-item) 10000) 100)]
         [expected-ledger-tag (+ statement-month (* 12 (- statement-year ledger-item-year)))]
         [s-expected-ledger-tag (fmt-i-02d expected-ledger-tag)]
         [signed-amount (ledger-signed-amount acct a-ledger-item)]
         [actual-ledger-tag
          (cond [(symbol=? acct-amount-match 'acct-amount-dr-and-cr-match)
                 (cond [(< signed-amount 0) (ledger-item-cr-seen a-ledger-item)]
                       [(> signed-amount 0) (ledger-item-dr-seen a-ledger-item)]
                       [else (let ([ctag (ledger-item-cr-seen a-ledger-item)]
                                   [dtag (ledger-item-dr-seen a-ledger-item)])
                               (if (string? ctag)
                                   (if (string? dtag)
                                       (if (string=? ctag dtag)
                                           ctag
                                           false)
                                       (error "do-ledger-statement-tags-match: No? ~a ~a [~a] ~a"
                                              (ledger-item-date a-ledger-item)
                                              (format-float (exact->inexact signed-amount 2))
                                              dtag ctag))
                                   (error "do-ledger-statement-tags-match: No? ~a ~a ~a [~a]"
                                          (ledger-item-date a-ledger-item)
                                          (format-float (exact->inexact signed-amount 2))
                                          dtag ctag)))])]
                [(symbol=? acct-amount-match 'acct-amount-dr-matches) (ledger-item-dr-seen a-ledger-item)]
                [(symbol=? acct-amount-match 'acct-amount-cr-matches) (ledger-item-cr-seen a-ledger-item)]
                [else false])])
    (and (string? actual-ledger-tag) (string=? actual-ledger-tag s-expected-ledger-tag))))

(define (appropriate-ledger-item-seen-tag acct a-ledger-item)
  (let ([signed-amount (ledger-signed-amount acct a-ledger-item)])
    (cond [(< signed-amount 0) (ledger-item-cr-seen a-ledger-item)]
          [(> signed-amount 0) (ledger-item-dr-seen a-ledger-item)]
          [else (let ([ctag (ledger-item-cr-seen a-ledger-item)]
                      [dtag (ledger-item-dr-seen a-ledger-item)])
                  (if (equal? ctag dtag)
                      ctag
                      false))])))

(define std-skip-tags (list "bf" "v"))

(define (sum-ledger-items acct ledger-items)
  (foldl + 0 (map (λ (li) (ledger-signed-amount acct li)) ledger-items)))

(define (pr-ledger-items acct ledger-items)
  (for-each (λ (li) (pr-ledger-item acct li)) ledger-items))

(define (pr-filtered-unmatched-ledger-items acct ymd8-end)
  (pr-ledger-items acct (filtered-unmatched-ledger-items acct ymd8-end)))

(define (filtered-unmatched-ledger-items acct ymd8-end)
  (let* ([a (unmatched-ledger-items-to-date acct ymd8-end)]
         [b (ledger-items-exclude-tags std-skip-tags acct a)]
         [c (ledger-items-exclude-prior-matches acct ymd8-end b)])
    c))

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

(define (ledger-items-exclude-prior-matches acct ymd8-end ledger-items)
  (filter (λ (a-ledger-item)
            (not (is-prior-month-ledger-item-match acct ymd8-end a-ledger-item)))
          ledger-items))

(define (is-prior-month-ledger-item-match acct ymd8-end a-ledger-item)
  (is-prior/later-month-ledger-item-match acct ymd8-end a-ledger-item <))

(define (is-later-month-ledger-item-match acct ymd8-end a-ledger-item)
  (is-prior/later-month-ledger-item-match acct ymd8-end a-ledger-item >))

(define (is-prior/later-month-ledger-item-match acct ymd8-end a-ledger-item op)
  (let ([yyyymm-current (year-month ymd8-end)])                         ; eg, 201602
    (let* ([yyyymmdd-li (ledger-item-date a-ledger-item)]               ; eg, 20160115
           [yyyymm-li (year-month (ledger-item-date a-ledger-item))]    ; eg, 201601
           [tag (appropriate-ledger-item-seen-tag acct a-ledger-item)]) ; eg, "01"
      (if (and (string? tag) (number? (string->number tag)))
          (op (effective-year-month yyyymmdd-li (string->number tag)) yyyymm-current)
          false))))

(define (outstanding-item-amount-as-of ymd8-end typ acct a-ledger-item)
  (let ([x-amount (ledger-item-amount a-ledger-item)]
        [x-dr-seen (ledger-item-dr-seen a-ledger-item)]
        [x-cr-seen (ledger-item-cr-seen a-ledger-item)])
    (cond [(symbol=? typ 'dr)
           (if (and (string=? acct (ledger-item-dr-acct a-ledger-item))
                    (is-outstanding-item-as-of acct ymd8-end a-ledger-item x-dr-seen))
               x-amount
               0)]
          [(symbol=? typ 'cr)
           (if (and (string=? acct (ledger-item-cr-acct a-ledger-item))
                    (is-outstanding-item-as-of acct ymd8-end a-ledger-item x-cr-seen))
               (- x-amount)
               0)]
          [else (error "outstanding-as-of: typ neither 'dr nor 'cr, instead got: " typ)])))

(define (is-outstanding-item-as-of acct ymd8-end a-ledger-item seen)
  (or (false? seen)
      (and (string? seen) (string=? seen ":"))
      (is-later-month-ledger-item-match acct ymd8-end a-ledger-item)))

(define (ledger-items-exclude-tags tags-to-exclude acct ledger-items)
  (filter (λ (li)
            (not (member (if (> (ledger-signed-amount acct li) 0)
                             (ledger-item-dr-seen li)
                             (ledger-item-cr-seen li))
                         tags-to-exclude)))
          ledger-items))

(define (unmatched-ledger-items-to-date acct ymd8-end)
  (let-values ([(statement-unmatched ledger-unmatched) (examine-acct acct ymd8-end)])
    (map (λ (x)
           (send x get-item))
         ledger-unmatched)))

(define (pr-examine-acct acct ymd8-end)
  (let-values ([(statement-unmatched ledger-unmatched) (examine-acct acct ymd8-end)])
    (printf "====== Statement items not in ledger:\n")
    (for-each (λ (x)
                (let ([si (send x get-item)])
                  (printf "~a ~a ~a\n" (statement-item-date si)
                          (~a (format-float
                               (exact->inexact (statement-item-amount si)) 2)
                              #:min-width 9 #:align 'right)
                          (statement-item-description si))))
              statement-unmatched)
    (printf "\n====== Ledger items not in loaded statements:\n")
    (for-each (λ (x)
                (let ([li (send x get-item)])
                  (pr-ledger-item acct li)))
              ledger-unmatched)))

(define (pr-ledger-item acct li)
  (printf "~a ~a ~a ~a / ~a\n"
          (ledger-item-date li)
          (~a (format-float
               (exact->inexact (ledger-signed-amount acct li)) 2)
              #:min-width 9 #:align 'right)
          (~a (if (> (ledger-signed-amount acct li) 0)
                  (ledger-item-dr-seen li)
                  (ledger-item-cr-seen li))
              #:min-width 2 #:align 'left)
          (ledger-item-payee li)
          (ledger-item-description li)))

(define (examine-acct acct ymd8-end)
  (let* ([statement-acct-items (statement-filter-acct acct (statement-range jan01 ymd8-end all-statement-items))]
         [ledger-acct-items (ledger-filter-acct acct (ledger-range jan01 ymd8-end all-ledger-items))]
         [statement-acct-track-items (map (λ (x) (new track-item [item x])) statement-acct-items)]
         [ledger-acct-track-items (map (λ (x) (new track-item [item x])) ledger-acct-items)])
    (for/list ([ledger-acct-track-item ledger-acct-track-items])
      (for/list ([statement-acct-track-item statement-acct-track-items])
        (check-ledger-statement-match
         ymd8-end ;; is this the right thing? it's really expecting the statement date
         acct ledger-acct-track-item statement-acct-track-item)))
    (values (filter (λ (x)
                      (let ([amount (statement-item-amount (send x get-item))])
                        (cond [(< amount 0) (send x cr-unmatched?)]
                              [(> amount 0) (send x dr-unmatched?)]
                              [else         (send x neither-dr-nor-cr-matched?)])))
                    statement-acct-track-items)
            (filter (λ (x)
                      (let ([amount (ledger-signed-amount acct (send x get-item))])1
                        (cond [(< amount 0) (send x cr-unmatched?)]
                              [(> amount 0) (send x dr-unmatched?)]
                              [else         (send x neither-dr-nor-cr-matched?)])))
                    ledger-acct-track-items))))

(define (amounts-new-dr-cr ymd8 typ acct ledger-items)
  (map (λ (li)
         (outstanding-item-amount-as-of ymd8 typ acct li))
       ledger-items))

(define (sum-amounts-new-cr acct stmt-ymd8 ledger-items)
  (apply + (amounts-new-dr-cr stmt-ymd8 'cr acct ledger-items)))

(define (sum-amounts-new-dr acct stmt-ymd8 ledger-items)
  (apply + (amounts-new-dr-cr stmt-ymd8 'dr acct ledger-items)))

; balance shown on statement
; + deposits in ledger that are not on statement
; - outstanding checks/withdrawals not shown on statement
; total should be same as statement

(define (pr-unreconciled acct ymd8)
  (let* ([reconciliation-ledger-items (db-get-reconciliation-ledger-items acct ymd8 (db-get-statement-balances acct))])
    (printf "\n======== ~a TOTAL ===== Debits Not Reconciled to a Statement\n"
            (~a (format-float
                 (sum-amounts-new-dr acct ymd8 reconciliation-ledger-items) 2)
                #:min-width 9 #:align 'right))
    (for-each (λ (x)
                (when (string=? acct (ledger-item-dr-acct x))
                  (printf "~a ~a ~a (~a) ~a / ~a\n"
                          (ledger-item-date x)
                          (~a
                           (format-float (exact->inexact (ledger-item-amount x)) 2)
                           #:min-width 9 #:align 'right)
                          (if (ledger-item-dr-seen x) ":" " ")
                          (ledger-item-cr-acct x)
                          (ledger-item-payee x)
                          (ledger-item-description x))))
              reconciliation-ledger-items)
    (printf "\n======== ~a TOTAL ===== Credits Not Reconciled to a Statement\n"
            (~a (format-float
                 (sum-amounts-new-cr acct ymd8 reconciliation-ledger-items) 2)
                #:min-width 9 #:align 'right))
    (for-each (λ (x)
                (when (string=? acct (ledger-item-cr-acct x))
                  (printf "~a ~a ~a (~a) ~a / ~a\n"
                          (ledger-item-date x)
                          (~a
                           (format-float (exact->inexact (- (ledger-item-amount x))) 2)
                           #:min-width 9 #:align 'right)
                          (if (ledger-item-cr-seen x) ":" " ")
                          (ledger-item-dr-acct x)
                          (ledger-item-payee x)
                          (ledger-item-description x))))
              reconciliation-ledger-items)))
