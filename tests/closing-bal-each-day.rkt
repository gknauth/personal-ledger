#lang racket

(require rackunit
         "../ledger.rkt")

(define given
  (list (day-bal 180401 20)
        (day-bal 180402 25) (day-bal 180402 27) (day-bal 180402 29) (day-bal 180402 30)
        (day-bal 180403 40) (day-bal 180404 -2) (day-bal 180404 15)
        (day-bal 180405 11) (day-bal 180405 -5)))

(define expected
  (list (day-bal 180401 20) (day-bal 180402 30) (day-bal 180403 40) (day-bal 180404 15) (day-bal 180405 -5)))

(check-equal? (closing-bal-each-day given) expected)

;;; ----------------------------------------------------------------------

(define mock-given
  (list (list 180401 20)
        (list 180402 25) (list 180402 27) (list 180402 29) (list 180402 30)
        (list 180403 40) (list 180404 -2) (list 180404 15)
        (list 180405 11) (list 180405 -5)))

(define mock-expected
  (list (list 180401 20) (list 180402 30) (list 180403 40) (list 180404 15) (list 180405 -5)))

(define (mock-closing-bal-each-day days-and-balances)
  (define (dotted-pair-to-list x) (list (car x) (cdr x)))
  (let* ([ht (for/hash ([i days-and-balances])
              (values (first i) (second i)))]
         [dotted-pairs (sequence->list (in-hash-pairs ht))])
    (map
     dotted-pair-to-list
     (sort
      dotted-pairs
      (λ (a b) (< (car a) (car b)))))))

(check-equal? (mock-closing-bal-each-day mock-given) mock-expected)
