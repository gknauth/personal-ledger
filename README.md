personal-ledger
===============

;; good for seeing if your ledger and external vendors' balances agree
(pr-acct-book-and-ext-balances-on-date acctid ymd8-end)

;; see which items in the ledger are still outstanding
(pr-outstanding-ledger-items acctid ymd8-end)

;; went and what is the minimum balance in this account looking forward ndays
(pr-min-acct-day-bal-forward acctid ndays)

;; good for finding which transactions on ledger cleared after statement date
(pr-stmt-unmatched-ledger-items acctid ymd8-end)

; not reconciled to a statement
(pr-unreconciled acctid ymd8)

