#lang racket

(require rackunit "../ledger.rkt" "sample-data.rkt")

(check-true
 (is-cleared-after-stmt
  "a foo" 20181018 (ledger-item 20180101 10.00 "a foo" "e tbd" "P00" "D00" #f ":" #f #f) ":"))

(check-false
 (is-cleared-after-stmt
  "a foo" 20181018 (ledger-item 20180101 10.00 "a foo" "e tbd" "P00" "D00" #f "10" #f #f) #f))

(check-false
 (is-ledger-outstanding-item-as-of
  "a foo" 20181018
  (ledger-item 20180101 10.00 "a foo" "e tbd" "P00" "D00" "bf" #f #f #f)))

(check-true
 (is-ledger-outstanding-item-as-of
  "a foo" 20181018
  (ledger-item 20180101 10.00 "a foo" "e tbd" "P00" "D00" #f #f #f #f)))

(check-equal?
 (format-ledger-item "a foo" (ledger-item 20180101 10.00 "a foo" "e tbd" "P00" "D00" "bf" #f #f #f))
 "20180101     10.00 bf P00 / D00\n")

(check-equal? (year-month 20181018) 201810)

(check-equal? (effective-year-month 20170202 01) 201701)
(check-equal? (effective-year-month 20160202 13) 201701)
(check-equal? (effective-year-month 20150202 25) 201701)
