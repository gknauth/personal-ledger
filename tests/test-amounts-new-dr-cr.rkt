#lang racket

(require rackunit
         "../reconciliation.rkt"
         "data/sample-a-bank-ledger-items.rkt")

(check-equal?
 (amounts-new-dr-cr 20160225 'cr "a BANK" 
                    (list (ledger-item 20160115 1 "x food" "a BANK" "P1" #f #f "02" #f #f)))
 (list 0))

(check-equal?
 (amounts-new-dr-cr 20160225 'cr "a BANK"  
                    (list (ledger-item 20160115 2 "x food" "a BANK" "P1" #f #f "03" #f #f)))
 (list -2))

(check-equal?
 (amounts-new-dr-cr 20160225 'dr "a BANK"  
                    (list (ledger-item 20160115 3 "a BANK" "a fund" "P1" #f "03" #f #f #f)))
 (list 3))

(check-equal?
 (amounts-new-dr-cr 20160225 'cr "a BANK"
                    (take sample-a-bank-ledger-items 9))
 (list 0 0 0
       0 0 -2.06
       0 -3.08 0))

(check-equal?
 (amounts-new-dr-cr 20160225 'cr "a BANK"
                    (takef sample-a-bank-ledger-items (lambda (li)
                                                        (and (>= (ledger-item-date li) 20160101)
                                                             (<= (ledger-item-date li) 20160331)))))
 (list 0 0 0
       0 0 -2.06
       0 -3.08 0))
