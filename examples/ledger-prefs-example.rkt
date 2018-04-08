#lang racket

(provide (all-defined-out))

(define start-year 2016)
(define end-year 2017)

(struct account (id color) #:transparent)

(define a-chk (account "a chk" "Forest Green"))
(define a-sav (account "a chk" "Turquoise"))

(define l-amex (account "l amex" "blue"))
(define l-visa (account "l visa" "red"))

(define cash-accounts (list a-chk a-sav))
(define debt-accounts (list l-amex l-visa))

(define accounts-to-show (append cash-accounts debt-accounts))

(define all-accounts (append cash-accounts debt-accounts))
