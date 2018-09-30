#lang racket/gui
;; personal-ledger/main.rkt
;; Copyright Geoffrey S. Knauth. See file "info.rkt".

(require table-panel
         format-ymd
         format-numbers
         "ledger.rkt"
         (file "~/.ledger-prefs.rkt"))

(define summary-frame
  (instantiate frame%
    ("Summary Information")))

(define detail-frame
  (instantiate frame%
    ("Detail Information")))

(define balance-frame
  (instantiate frame%
    ("Balances")))

(define summary-cols (vector "acct" "date" "book" "ext" "(- book ext)" "outstanding"
                             "stmt" "stmt-bal" "sync"
                             "new-dr" "new-cr" "reconciliation" "unmatched" "should-match"))
(define summary-rows
  (list->vector (cons empty (map (λ (x) (account-id x)) accounts-to-show))))

(define summary-cells (make-vector (* (vector-length summary-cols) (vector-length summary-rows))))

(define detail-rows (vector "acct" "date" "book" "ext" "(- book ext)" "outstanding"
                            "stmt" "stmt-bal" "sync"
                            "new-dr" "new-cr" "reconciliation" "unmatched" "should-match"))

(define detail-cols (make-vector 2))

(define detail-cells (make-vector (* (vector-length detail-cols) (vector-length detail-rows))))
  
(define balance-cols (vector "acct" "date" "ext"))

(define balance-rows
  (list->vector (cons empty (map (λ (x) (account-id x)) accounts-to-show))))

(define balance-cells (make-vector (* (vector-length balance-cols) (vector-length balance-rows))))

; get the column index for the name, -1 if no such column name
; string -> integer
(define (summary-col-name-index name)
  (define (helper name lst i)
    (cond [(empty? lst) -1]
          [(if (string=? name (first lst))
               i
               (helper name (rest lst) (+ 1 i)))]))
  (helper name (vector->list summary-cols) 0))

; get the row index for the name, -1 if no such column name
; string -> integer
(define (detail-row-name-index name)
  (define (helper name lst i)
    (cond [(empty? lst) -1]
          [(if (string=? name (first lst))
               i
               (helper name (rest lst) (+ 1 i)))]))
  (helper name (vector->list detail-rows) 0))

(define summary-table-panel
  (instantiate table-panel%
    (summary-frame)
    (dimensions `(,(+ 1 (vector-length summary-rows)) ,(vector-length summary-cols)))
    (column-stretchability #t)
    (row-stretchability #f)))

(define detail-table-panel
  (instantiate table-panel%
    (detail-frame)
    (dimensions `(,(+ 1 (vector-length detail-rows)) ,(vector-length detail-cols)))
    (column-stretchability #t)
    (row-stretchability #f)))

(define balance-table-panel
  (instantiate table-panel%
    (balance-frame)
    (dimensions `(,(+ 1 (vector-length balance-rows)) ,(vector-length balance-cols)))
    (column-stretchability #t)
    (row-stretchability #f)))

; get the 1D vector index given 2D row col
; integer integer -> integer
(define (summary-ij row col)
  (+ (* row (vector-length summary-cols)) col))

(define (detail-ij row col)
  (+ (* row (vector-length detail-cols)) col))

(define (balance-ij row col)
  (+ (* row (vector-length balance-cols)) col))

; integer string -> integer
(define (summary-ij-s row col-name)
  (let ([col (summary-col-name-index col-name)])
    (if (= -1 col)
        (error "col-name ~a is invalid" col-name)
        (summary-ij row col))))

; string integer -> integer
(define (detail-ij-s row-name col)
  (let ([row (detail-row-name-index row-name)])
    (if (= -1 row)
        (error "row-name ~a is invalid" row-name)
        (detail-ij row col))))

(define text-field-right-align%
  (class text-field%
    (inherit get-editor get-client-size)
    (define/private (reset-width)
      (define e (get-editor))
      (define wb (box 0.0))
      (send (send e get-admin) get-view #f #f wb #f)
      (send e set-max-width (- (unbox wb) 2))
      (send e set-paragraph-alignment 0 'right))
    (define/override (on-size w h)
      (super on-size w h)
      (reset-width))
    (init [callback void])
    (super-new [callback (lambda (t e)
                           ;; an editor tends to lose
                           ;; its alignment if all the text
                           ;; is deleted, so reset it
                           ;; after any change
                           (reset-width)
                           (callback t e))])
    (reset-width)))

(define all-stmt-bals (make-hash))

(define (load-balances)
  (for ([row (in-range (vector-length summary-rows))])
    (let ([acct (vector-ref summary-rows row)])
      (when (not (null? acct))
        (let ([bals (db-get-statement-balances acct)])
          (hash-set! all-stmt-bals acct bals))))))

(define std-col-width 60)

(define (setup-summary-cells)
  (for ([row (in-range (vector-length summary-rows))])
    (let ([acct (vector-ref summary-rows row)])
      (for ([col (in-range (vector-length summary-cols))])
        (vector-set! summary-cells (summary-ij row col) 
                     (if (= 0 row)
                         (new message%
                              (parent summary-table-panel)
                              (label (vector-ref summary-cols col))
                              (min-width std-col-width))
                         (cond [(= col (summary-col-name-index "stmt"))
                                (new combo-field%
                                     (parent summary-table-panel)
                                     (label "")
                                     (choices (get-stmt-dates acct))
                                     (callback (λ (t e)
                                                 (summary-stmt-date-changed t e row)))
                                     (min-width std-col-width))]
                               [(= col (summary-col-name-index "sync"))
                                (new button%
                                     (label "◀")
                                     (parent summary-table-panel)
                                     (min-width 50)
                                     (callback (λ (t e)
                                                 (summary-sync-button-pressed t e row))))]
                               [else
                                (new text-field%
                                     (parent summary-table-panel)
                                     (label "")
                                     (init-value (cond [(= 0 col) acct]
                                                       [(= 1 col) (number->string (today->ymd8))]
                                                       [else ""]))
                                     (enabled (= col (summary-col-name-index "date")))
                                     (callback (cond [(= col (summary-col-name-index "date"))
                                                      (λ (t e)
                                                        (summary-date-changed t e row))]
                                                     [else (λ (t e ) (void))]))
                                     (min-width 120))])
                         ))))))

(define (detail-date-ymd8)
  (text-field->number (detail-cell-named "date")))

(define (detail-selected-acct)
  (choice->string (detail-cell-named "acct")))

(define (detail-stmt-date-ymd8)
  (choice->number (detail-cell-named "stmt")))

(define (detail-cell-named name)
  (vector-ref detail-cells (detail-ij-s name 0)))

(define (choice->string choice)
  (send choice get-string (send choice get-selection)))

(define (choice->number choice)
  (string->number (choice->string choice)))

(define (text-field->string text-field)
  (send text-field get-value))

(define (text-field->number text-field)
  (string->number (text-field->string text-field)))

(define (set-text-field-number! text-field num)
  (set-text-field-string! text-field (number->string num)))

(define (set-text-field-string! text-field str)
  (send text-field set-value str))

(define (setup-detail-cells)
  (for ([row (in-range (vector-length detail-rows))])
    (for ([col (in-range (vector-length detail-cols))])
      (cond [(= 0 col)
             (vector-set! detail-cells (detail-ij row col)
                          (cond [(= row (detail-row-name-index "acct"))
                                 (new choice%
                                      (parent detail-table-panel)
                                      (label (vector-ref detail-rows 0))
                                      (choices (map (λ (x) (account-id x)) all-accounts))
                                      (callback (λ (t e) (detail-acct-changed t e))))]
                                [(= row (detail-row-name-index "date"))
                                 (new text-field%
                                      (parent detail-table-panel)
                                      (label "date")
                                      (enabled #t)
                                      (callback (λ (t e) (detail-date-changed t e)))
                                      (init-value (cond [(= row (detail-row-name-index "date"))
                                                         (number->string (today->ymd8))]
                                                        [else ""])))]
                                [(= row (detail-row-name-index "stmt"))
                                 (new choice%
                                      (parent detail-table-panel)
                                      (label "stmt")
                                      (choices (get-stmt-dates (detail-selected-acct)))
                                      (callback (λ (t e)
                                                  (detail-stmt-date-changed t e)))
                                      (min-width std-col-width))]
                                [(= row (detail-row-name-index "sync"))
                                 (new button%
                                      (label "sync")
                                      (parent detail-table-panel)
                                      (callback (λ (t e)
                                                  (detail-sync-button-pressed t e))))]
                                [else
                                 (new text-field%
                                      (parent detail-table-panel)
                                      (label (vector-ref detail-rows row))
                                      (init-value ""))]))]
            [(= 1 col)
             (vector-set! detail-cells (detail-ij row col)
                          (cond [(= row (detail-row-name-index "date"))
                                 (new button%
                                      (parent detail-table-panel)
                                      (label "plot")
                                      (callback (λ (t e)
                                                  (plot-day-bals-forward
                                                   (hash-ref accounts-ht (detail-selected-acct))
                                                   60))))]
                                [(= row (detail-row-name-index "outstanding"))
                                 (new button%
                                      (parent detail-table-panel)
                                      (label "show")
                                      (callback (λ (t e)
                                                  (show-outstanding-ledger-items
                                                   (detail-selected-acct)
                                                   (detail-date-ymd8)))))]
                                [(= row (detail-row-name-index "unmatched"))
                                 (new button%
                                      (parent detail-table-panel)
                                      (label "show")
                                      (callback (λ (t e)
                                                  (show-unmatched-ledger-items
                                                   (detail-selected-acct)
                                                   (detail-stmt-date-ymd8)))))]
                                [else (new message%
                                           (parent detail-table-panel)
                                           (label ""))]))]
            [else (error "setup-detail-cells should not get here")]))))

(define (mk-balance-outstanding-frame)
  (instantiate frame% ("Outstanding Transactions")))

(define (mk-unmatched-items-frame)
  (instantiate frame% ("Unmatched Items")))

(define (mk-balance-outstanding-panel parent-frame)
  (instantiate panel%
    [parent-frame]
    [min-width 800]
    [min-height 250]
    [style (list 'vscroll)]))

(define (mk-unmatched-items-panel parent-frame)
  (instantiate panel%
    [parent-frame]
    [min-width 800]
    [min-height 250]
    [style (list 'vscroll)]))

(define (mk-balance-outstanding-text-field parent-panel acctid font-size text)
  (new text-field%
       [label acctid]
       [parent parent-panel]
       [font (make-object font% font-size 'modern)]
       [init-value text]
       [style (list 'multiple 'vertical-label)]
       [stretchable-width #t]
       [stretchable-height #t]))

(define (show-outstanding-ledger-items acctid ymd8)
  (let* ([frame (mk-balance-outstanding-frame)]
         [panel (mk-balance-outstanding-panel frame)]
         [textfield (mk-balance-outstanding-text-field panel acctid 14
                                                       (format-outstanding-ledger-items acctid ymd8))])                        
    (send frame show #t)))

(define (show-unmatched-ledger-items acctid ymd8)
  (let* ([frame (mk-unmatched-items-frame)]
         [panel (mk-unmatched-items-panel frame)]
         [textfield (mk-balance-outstanding-text-field panel acctid 14
                                                       (format-stmt-unmatched-ledger-items acctid ymd8))])                        
    (send frame show #t)))

(define (update-all-summary-rows)
  (for ([row (in-range (vector-length summary-rows))])
    (when (> row 0)
      (let ([t (vector-ref summary-cells (summary-ij-s row "date"))])
        (summary-update-row t row)))))

(define (detail-update)
  (let ([ymd8 (text-field->number (detail-cell-named "date"))])
    (detail-update-book-ext-diff-outstanding-as-of ymd8)
    (detail-update-stmt-choices)
    (detail-update-stmt-bal)))

(define (detail-sync)
  (let ([ymd8 (choice->number (detail-cell-named "stmt"))])
    (detail-update-date ymd8)
    (detail-update-book-ext-diff-outstanding-as-of ymd8)
    (detail-update-stmt-bal)))

(define (detail-update-stmt-choices)
  (let ([choice-o (detail-choice-o-named "stmt")])
    (send choice-o clear)
    (for-each (λ (x)
                (send choice-o append x))
              (get-stmt-dates (detail-selected-acct)))))

(define (detail-cell-named-in-col name col)
  (vector-ref detail-cells (detail-ij-s name col)))

;(define (detail-update-stmt-bal)
;  (let ([acct (get-detail-selected-acct)]
;        [s-which-stmt-date (detail-value-choice-o-named "stmt")])
;    (send (detail-cell-named-in-col "stmt-bal" 0) set-value (acct-bal-str acct s-which-stmt-date))))

(define (acct-bal-str acct s-stmt-date)
  (let ([acct-date-bals (hash-ref all-stmt-bals acct)])
    (if (= (string-length s-stmt-date) 8)
        (let ([bal (get-stmt-bal-for-date acct-date-bals (string->number s-stmt-date))])
          (if bal (format-exact bal 2) "n/a"))
        "")))

;; TODO do we need this?
(define (detail-choice-o-named name)
  (vector-ref detail-cells (detail-ij (detail-row-name-index name) 0)))

;; TODO do we need this?
(define (detail-value-choice-o-named name)
  (let* ([choice-o (detail-choice-o-named name)]
         [i (send choice-o get-selection)])
    (if i (send choice-o get-string i) "")))

(define (summary-update-row t row)
  (let ([s-as-of (send t get-value)])
    (when (> (string-length s-as-of) 0)
      (summary-update-book-ext-diff-outstanding row (string->number s-as-of)))))

(define (summary-update-book-ext-diff-outstanding row ymd8)
  (let* ([acctid (send (vector-ref summary-cells (summary-ij-s row "acct")) get-value)]
         [book-ext-diff (get-book-ext-diff acctid ymd8)]
         [book (first book-ext-diff)]
         [ext (second book-ext-diff)]
         [book-minus-ext (third book-ext-diff)]
         [outstanding (sum-outstanding-ledger-items acctid ymd8)])
    (send (vector-ref summary-cells (summary-ij-s row "book")) set-value (format-exact book 2))
    (send (vector-ref summary-cells (summary-ij-s row "ext")) set-value (format-exact ext 2))
    (send (vector-ref summary-cells (summary-ij-s row "(- book ext)"))
          set-value (format-exact book-minus-ext 2))
    (send (vector-ref summary-cells (summary-ij-s row "outstanding"))
          set-value (format-exact outstanding 2))))

(define (detail-update-outstanding ymd8)
  (let* ([outstanding (sum-outstanding-ledger-items (detail-selected-acct) ymd8)])
    (set-text-field-string! (detail-cell-named "outstanding") (format-exact outstanding 2))))

(define (summary-date-changed t e row)
  (when (eq? (send e get-event-type) 'text-field-enter)
    (summary-update-row t row)))

(define (detail-date-changed t e)
  (when (eq? (send e get-event-type) 'text-field-enter)
    (detail-update)))

(define (summary-sync-button-pressed t e row)
  (summary-sync-date-book-ext-diff row)
  (summary-update-seen-also row))

(define (detail-sync-button-pressed t e)
  (detail-sync)
  (detail-update-seen-also))

(define (get-stmt-dates acct)
  (reverse (map (λ (x) (number->string (first x)))
                (hash-ref all-stmt-bals acct))))

(define (summary-update-seen-also row)
  (let* ([acct (vector-ref summary-rows row)]
         [s-which-stmt-date (send (vector-ref summary-cells (summary-ij-s row "stmt")) get-value)]
         [stmt-ymd8 (string->number s-which-stmt-date)]
         [s-stmt-bal (send (vector-ref summary-cells (summary-ij-s row "stmt-bal")) get-value)])
    (when (and (= (string-length s-which-stmt-date) 8)
               (> (string-length s-stmt-bal) 0))
      (let* ([ext (string->number (send (vector-ref summary-cells (summary-ij-s row "ext")) get-value))]
             [stmt-bal (string->number (if (string=? s-stmt-bal "n/a") "0" s-stmt-bal))]
             [dr-cr-rec-diff (get-new-dr-new-cr-reconciliation-diff acct stmt-ymd8 ext)]
             [new-dr (first dr-cr-rec-diff)]
             [new-cr (second dr-cr-rec-diff)]
             [reconciliation (third dr-cr-rec-diff)]
             [ext-minus-reconciliation (fourth dr-cr-rec-diff)])
        (send (vector-ref summary-cells (summary-ij-s row "new-dr")) set-value
              (format-exact new-dr 2))
        (send (vector-ref summary-cells (summary-ij-s row "new-cr")) set-value
              (format-exact new-cr 2))
        (when (not (string=? s-stmt-bal "n/a"))
          (send (vector-ref summary-cells (summary-ij-s row "reconciliation")) set-value
                (format-exact reconciliation 2))
          (let ([unmatched (sum-ledger-items acct (filtered-stmt-unmatched-ledger-items acct stmt-ymd8))])
            (send (vector-ref summary-cells (summary-ij-s row "unmatched")) set-value
                  (format-exact unmatched 2))
            (send (vector-ref summary-cells (summary-ij-s row "should-match")) set-value
                  (format-exact (+ reconciliation unmatched) 2))))))))

(define (detail-update-seen-also)
  (let* ([col 0]
         [acct (detail-selected-acct)]
         [s-which-stmt-date (detail-value-choice-o-named "stmt")]
         [stmt-ymd8 (string->number s-which-stmt-date)]
         [s-stmt-bal (text-field->string (detail-cell-named "stmt-bal"))])
    (when (and (= (string-length s-which-stmt-date) 8)
               (> (string-length s-stmt-bal) 0))
      (let* ([ext (text-field->number (detail-cell-named "stmt-bal"))]
             [stmt-bal (string->number (if (string=? s-stmt-bal "n/a") "0" s-stmt-bal))]
             [dr-cr-rec-diff (get-new-dr-new-cr-reconciliation-diff acct stmt-ymd8 ext)]
             [new-dr (first dr-cr-rec-diff)]
             [new-cr (second dr-cr-rec-diff)]
             [reconciliation (third dr-cr-rec-diff)]
             [ext-minus-reconciliation (fourth dr-cr-rec-diff)])
        (set-text-field-string! (detail-cell-named "new-dr") (format-exact new-dr 2))
        (set-text-field-string! (detail-cell-named "new-cr") (format-exact new-cr 2))
        (when (not (or (string=? s-stmt-bal "n/a") (string=? s-stmt-bal "")))
          (set-text-field-string! (detail-cell-named "reconciliation") (format-exact reconciliation 2))
          (let ([unmatched (sum-ledger-items acct (filtered-stmt-unmatched-ledger-items acct stmt-ymd8))])
        (set-text-field-string! (detail-cell-named "unmatched")
                                (format-exact unmatched 2))
        (set-text-field-string! (detail-cell-named "should-match")
                                (format-exact (+ reconciliation unmatched) 2))))))))

(define (summary-sync-date-book-ext-diff row)
  (let* ([acct (vector-ref summary-rows row)]
         [s-which-stmt-date (send (vector-ref summary-cells (summary-ij-s row "stmt")) get-value)])
    (when (= (string-length s-which-stmt-date) 8)
      (send (vector-ref summary-cells (summary-ij-s row "date")) set-value s-which-stmt-date)
      (summary-update-book-ext-diff-outstanding row (string->number s-which-stmt-date)))))

(define (detail-update-book-ext-diff-outstanding-as-of ymd8)
    (let* ([col 0]
           [acctid (detail-selected-acct)]
           [book-ext-diff (get-book-ext-diff acctid ymd8)]
           [book (first book-ext-diff)]
           [ext (second book-ext-diff)]
           [book-minus-ext (third book-ext-diff)]
           [outstanding (sum-outstanding-ledger-items acctid ymd8)])
    (send (vector-ref detail-cells (detail-ij-s "book" col)) set-value (format-exact book 2))
    (send (vector-ref detail-cells (detail-ij-s "ext" col)) set-value (format-exact ext 2))
    (send (vector-ref detail-cells (detail-ij-s "(- book ext)" col))
          set-value (format-exact book-minus-ext 2))
    (send (vector-ref detail-cells (detail-ij-s "outstanding" col))
          set-value (format-exact outstanding 2))))

(define (summary-stmt-date-changed t e row)
  (summary-update-stmt-bal t row))

(define (detail-stmt-date-changed t e)
  (detail-update-stmt-bal)
  (detail-clear-stmt-dependents))

(define (detail-acct-changed t e)
  (detail-update))

(define (summary-update-stmt-bal t row)
  (let* ([acct (vector-ref summary-rows row)]
         [s-which-stmt-date (send (vector-ref summary-cells (summary-ij-s row "stmt")) get-value)])
    (send (vector-ref summary-cells (summary-ij-s row "stmt-bal")) set-value
          (acct-bal-str acct s-which-stmt-date))
    (for/list ([colname (list "new-cr" "new-dr" "reconciliation" "unmatched" "should-match")])
      (send (vector-ref summary-cells (summary-ij-s row colname)) set-value ""))))

(define (detail-update-date ymd8)
  (let ([text-field (detail-cell-named "date")])
    (set-text-field-number! text-field ymd8)))

(define (detail-update-stmt-bal)
  (let* ([col 0]
         [acct (detail-selected-acct)]
         [acct-date-bals (hash-ref all-stmt-bals acct)]
         [s-which-stmt-date (detail-value-choice-o-named "stmt")])
    (send (vector-ref detail-cells (detail-ij-s "stmt-bal" col)) set-value
          (if (= (string-length s-which-stmt-date) 8)
              (let ([bal (get-stmt-bal-for-date acct-date-bals (string->number s-which-stmt-date))])
                (if bal (format-exact bal 2) "n/a"))
              ""))
    (for/list ([rowname (list "new-cr" "new-dr" "reconciliation" "unmatched" "should-match")])
      (send (vector-ref detail-cells (detail-ij-s rowname col)) set-value ""))))

(define (detail-clear-stmt-dependents)
  (let* ([col 0])
    (for/list ([rowname (list "new-cr" "new-dr" "reconciliation" "unmatched" "should-match")])
      (send (vector-ref detail-cells (detail-ij-s rowname col)) set-value ""))))
  
(define setup-gui-already-called #f)

(define (setup-gui)
  (if setup-gui-already-called
      (error "setup-gui was already called")
      (begin
        (load-balances)
        (setup-summary-cells)
        (setup-detail-cells)
        (update-all-summary-rows)
        (detail-update))))

(define (refresh-gui)
  (refresh)
  (load-balances)
  (update-all-summary-rows)
  (detail-update))

(define (show-gui)
  (send summary-frame show #t)
  (send detail-frame show #t))

(define (hide-gui)
  (send summary-frame show #f)
  (send detail-frame show #f))

(setup-gui)
(show-gui)

(module+ test
  (require rackunit)
  (require "tests/amounts-new-dr-cr.rkt")
  (require "tests/sum-ledger-items.rkt")
  (require "tests/closing-bal-each-day.rkt")
  )

(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html



  
