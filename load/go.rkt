#!/usr/sbin/racket
#lang racket

(require racket/cmdline
         "load.rkt")

(define year 0)
(command-line #:args (year) (generate-ledger-sql-file (string->number year)))
