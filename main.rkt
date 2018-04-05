#lang racket/gui
;; personal-ledger/main.rkt
;; Copyright Geoffrey S. Knauth. See file "info.rkt".

(require table-panel
         format-ymd
         format-numbers
         "ledger.rkt"
         (file "~/.ledger-prefs.rkt"))

(define frame
  (instantiate frame%
    ("Financial Dashboard")))

(define cols (vector "acct" "date" "book" "ext" "(- ext book)"
                     "stmt" "stmt-bal" "sync"
                     "new-dr" "new-cr" "reconciliation" "more-seen" "should-match"))
(define rows (list->vector (cons empty accounts-to-show)))
(define cells (make-vector (* (vector-length cols) (vector-length rows))))

; get the column index for the name, -1 if no such column name
; string -> integer
(define (col-name-index name)
  (define (helper name lst i)
    (cond [(empty? lst) -1]
          [(if (string=? name (first lst))
               i
               (helper name (rest lst) (+ 1 i)))]))
  (helper name (vector->list cols) 0))

(define table-panel
  (instantiate table-panel%
    (frame)
    ;(style '(border))
    (dimensions `(,(+ 1 (vector-length rows)) ,(vector-length cols)))
    (column-stretchability #t)
    (row-stretchability #f)))

; get the 1D vector index given 2D row col
; integer integer -> integer
(define (ij row col)
  (+ (* row (vector-length cols)) col))

; integer string -> integer
(define (ij-s row col-name)
  (let ([col (col-name-index col-name)])
    (if (= -1 col)
        (error "col-name ~a is invalid" col-name)
        (ij row col))))

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

(define (setup-stmt-bals)
  (for ([row (in-range (vector-length rows))])
    (let ([acct (vector-ref rows row)])
      (when (not (null? acct))
        (let ([bals (db-get-statement-balances acct)])
          (hash-set! all-stmt-bals acct bals))))))

(define std-col-width 60)

(define (setup-cells)
  (for ([row (in-range (vector-length rows))])
    (let ([acct (vector-ref rows row)])
      (for ([col (in-range (vector-length cols))])
        (vector-set! cells (ij row col) 
                     (if (= 0 row)
                         (new message%
                              (parent table-panel)
                              (label (vector-ref cols col))
                              (min-width std-col-width))
                         (cond [(= col (col-name-index "stmt")) (new combo-field%
                                                                     (parent table-panel)
                                                                     (label "")
                                                                     (choices (reverse (map (λ (x) (number->string (first x)))
                                                                                            (hash-ref all-stmt-bals acct))))
                                                                     (callback (λ (t e)
                                                                                 (stmt-date-changed t e row)))
                                                                     (min-width std-col-width))]
                               [(= col (col-name-index "sync")) (new button%
                                                                     (label "◀")
                                                                     (parent table-panel)
                                                                     (min-width 50)
                                                                     (callback (λ (t e)
                                                                                 (sync-button-pressed t e row))))]
                               [else (new text-field%
                                          (parent table-panel)
                                          (label "")
                                          (init-value (cond [(= 0 col) acct]
                                                            [(= 1 col) (number->string (today->ymd8))]
                                                            [else ""]))
                                          (enabled (= col (col-name-index "date")))
                                          (callback (cond [(= col (col-name-index "date"))
                                                           (λ (t e)
                                                             (date-changed t e row))]
                                                          [else (λ (t e ) (void))]))
                                          (min-width 120))])
                         ))))))

(define (update-all-rows)
  (for ([row (in-range (vector-length rows))])
    (when (> row 0)
      (let ([t (vector-ref cells (ij-s row "date"))])
        (update-row t row)))))

(define (update-row t row)
  (let ([s-as-of (send t get-value)])
    (when (> (string-length s-as-of) 0)
      (update-book-ext-diff row (string->number s-as-of)))))

(define (update-book-ext-diff row ymd8)
  (let* ([acct (send (vector-ref cells (ij-s row "acct")) get-value)]
         [book-ext-diff (get-book-ext-diff acct ymd8)]
         [book (first book-ext-diff)]
         [ext (second book-ext-diff)]
         [ext-minus-book (third book-ext-diff)])
    (send (vector-ref cells (ij-s row "book")) set-value (format-exact book 2))
    (send (vector-ref cells (ij-s row "ext")) set-value (format-exact ext 2))
    (send (vector-ref cells (ij-s row "(- ext book)"))
            set-value (format-exact ext-minus-book 2))))

(define (date-changed t e row)
  (when (eq? (send e get-event-type) 'text-field-enter)
    (update-row t row)))

(define (sync-button-pressed t e row)
  (update-date-book-ext-diff row)
  (update-seen-also row))

(define (update-seen-also row)
  (let* ([acct (vector-ref rows row)]
         [s-which-stmt-date (send (vector-ref cells (ij-s row "stmt")) get-value)]
         [stmt-ymd8 (string->number s-which-stmt-date)]
         [s-stmt-bal (send (vector-ref cells (ij-s row "stmt-bal")) get-value)])
    (when (and (= (string-length s-which-stmt-date) 8)
               (> (string-length s-stmt-bal) 0))
      (let* ([ext (string->number (send (vector-ref cells (ij-s row "ext")) get-value))]
             [stmt-bal (string->number (if (string=? s-stmt-bal "n/a") "0" s-stmt-bal))]
             [dr-cr-rec-diff (get-new-dr-new-cr-reconciliation-diff acct stmt-ymd8 ext)]
             [new-dr (first dr-cr-rec-diff)]
             [new-cr (second dr-cr-rec-diff)]
             [reconciliation (third dr-cr-rec-diff)]
             [ext-minus-reconciliation (fourth dr-cr-rec-diff)])
        (send (vector-ref cells (ij-s row "new-dr")) set-value
              (format-exact new-dr 2))
        (send (vector-ref cells (ij-s row "new-cr")) set-value
              (format-exact new-cr 2))
        (when (not (string=? s-stmt-bal "n/a"))
          (send (vector-ref cells (ij-s row "reconciliation")) set-value
                (format-exact reconciliation 2))
          (let ([more-seen (sum-ledger-items acct (filtered-unmatched-ledger-items acct stmt-ymd8))])
            (send (vector-ref cells (ij-s row "more-seen")) set-value
                  (format-exact more-seen 2))
            (send (vector-ref cells (ij-s row "should-match")) set-value
                  (format-exact (+ reconciliation more-seen) 2))))))))

(define (update-date-book-ext-diff row)
  (let* ([acct (vector-ref rows row)]
         [s-which-stmt-date (send (vector-ref cells (ij-s row "stmt")) get-value)])
    (when (= (string-length s-which-stmt-date) 8)
      (send (vector-ref cells (ij-s row "date")) set-value s-which-stmt-date)
      (update-book-ext-diff row (string->number s-which-stmt-date)))))

(define (stmt-date-changed t e row)
  (update-stmt-bal t row))

(define (update-stmt-bal t row)
  (let* ([acct (vector-ref rows row)]
         [acct-date-bals (hash-ref all-stmt-bals acct)]
         [s-which-stmt-date (send (vector-ref cells (ij-s row "stmt")) get-value)])
    (send (vector-ref cells (ij-s row "stmt-bal")) set-value
          (if (= (string-length s-which-stmt-date) 8)
              (let ([bal (get-stmt-bal-for-date acct-date-bals (string->number s-which-stmt-date))])
                (if bal (format-exact bal 2) "n/a"))
              ""))
    (for/list ([colname (list "new-cr" "new-dr" "reconciliation" "more-seen" "should-match")])
      (send (vector-ref cells (ij-s row colname)) set-value ""))))

(setup-stmt-bals)
(setup-cells)
(update-all-rows)

(define (show-dashboard)
  (send frame show #t))

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



