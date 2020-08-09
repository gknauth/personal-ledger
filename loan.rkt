#lang racket

(provide compute-payment
         compute-monthly-payment
         compute-remaining-monthly-payment
         compute-remaining-daily-payment)

(define (compute-payment initial-balance
                         annual-interest-rate
                         payments-per-year
                         total-number-of-payments)
  (let ([i (/ (if (> annual-interest-rate 0)
                  annual-interest-rate
                  0.0000001)
              payments-per-year)])
    (/ (* i initial-balance)
       (- 1 (expt (+ 1 i) (- total-number-of-payments))))))

(define (compute-monthly-payment initial-balance
                                 annual-interest-rate
                                 months-to-pay)
  (compute-payment initial-balance annual-interest-rate 12 months-to-pay))

(define (compute-remaining-monthly-payment initial-balance
                                           annual-interest-rate
                                           months-to-pay
                                           already-paid-this-month)
  (- (compute-monthly-payment initial-balance
                              annual-interest-rate
                              months-to-pay)
     already-paid-this-month))

(define average-days-in-month
  (/ (+ 31 28.25 31 30 31 30 31 31 30 31 30 31) 12))

(define (compute-remaining-daily-payment initial-balance
                                         annual-interest-rate
                                         months-to-pay
                                         already-paid-this-month)
  (/ (compute-remaining-monthly-payment initial-balance
                                        annual-interest-rate
                                        months-to-pay
                                        already-paid-this-month)
     average-days-in-month))