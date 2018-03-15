#lang racket

(require "../../ledger.rkt")
(provide sample-a-bank-ledger-items)

(define sample-a-bank-ledger-items
  (list

   ;; If you're wondering why BANK is capitalized (it's doesn't have
   ;; to be) it was just to make it easier for me to see where I
   ;; needed to choose test values for dr-seen and cr-seen, the 4th
   ;; and 3rd to last columns.

   (ledger-item 20160115  1.01 "a BANK" "a foo"  "P01" "D01"   "01" #f      #f #f)
   (ledger-item 20160120  1.02 "a foo"  "a BANK" "P02" "D02"   #f   "01"    #f #f)
   (ledger-item 20160125  1.03 "a BANK" "a foo"  "P03" "D03"   "01" #f      #f #f)

   (ledger-item 20160214  2.04 "a foo"  "a BANK" "P04" "D04"   #f   "02"    #f #f)
   (ledger-item 20160220  2.05 "a BANK" "a foo"  "P05" "D05"   ":"  #f      #f #f)
   (ledger-item 20160226  2.06 "a foo"  "a BANK" "P06" "D06"   #f  "03"     #f #f)

   (ledger-item 20160315  3.07 "a BANK" "a foo"  "P07" "D07"   "03" #f      #f #f)
   (ledger-item 20160320  3.08 "a foo"  "a BANK" "P08" "D08"   #f   "03"    #f #f)
   (ledger-item 20160325  3.09 "a BANK" "a foo"  "P09" "D09"   "03" #f      #f #f)

   (ledger-item 20160414  4.10 "a foo"  "a BANK" "P10" "D10"   #f   "04"    #f #f)
   (ledger-item 20160420  4.11 "a BANK" "a foo"  "P11" "D11"   "04" #f      #f #f)
   (ledger-item 20160426  4.12 "a foo"  "a BANK" "P12" "D12"   #f   "04"    #f #f)

   (ledger-item 20160515  5.13 "a BANK" "a foo"  "P13" "D13"   "05" #f      #f #f)
   (ledger-item 20160520  5.14 "a foo"  "a BANK" "P14" "D14"   #f   "05"    #f #f)
   (ledger-item 20160525  5.15 "a BANK" "a foo"  "P15" "D15"   "06" #f      #f #f)

   (ledger-item 20160614  6.16 "a foo"  "a BANK" "P16" "D16"   "06"  #f     #f #f)
   (ledger-item 20160620  6.17 "a BANK" "a foo"  "P17" "D17"   #f   "06"    #f #f)
   (ledger-item 20160626  6.18 "a foo"  "a BANK" "P18" "D18"   "07" #f      #f #f)

   (ledger-item 20160715  7.19 "a BANK" "a foo"  "P19" "D19"   "07" "07"    #f #f)
   (ledger-item 20160720  7.20 "a foo"  "a BANK" "P20" "D20"   #f   "07"    #f #f)
   (ledger-item 20160725  7.21 "a BANK" "a foo"  "P21" "D21"   "07" #f      #f #f)

   (ledger-item 20160815  8.22 "a BANK" "a foo"  "P22" "D22"   "08" #f      #f #f)
   (ledger-item 20160820  8.23 "a foo"  "a BANK" "P23" "D23"   #f   ":"     #f #f)
   (ledger-item 20160825  8.24 "a BANK" "a foo"  "P24" "D24"   "09" #f      #f #f)

   (ledger-item 20160915  9.25 "a BANK" "a foo"  "P25" "D25"   "09" #f      #f #f)
   (ledger-item 20160920  9.26 "a foo"  "a BANK" "P26" "D26"   #f   "09"    #f #f)
   (ledger-item 20160925  9.27 "a BANK" "a foo"  "P27" "D27"   "09" #f      #f #f)

   (ledger-item 20161015 10.28 "a BANK" "a foo"  "P28" "D28"   "10" #f      #f #f)
   (ledger-item 20161020 10.29 "a foo"  "a BANK" "P29" "D29"   #f   "12"    #f #f)
   (ledger-item 20161025 10.30 "a BANK" "a foo"  "P30" "D30"   "10" #f      #f #f)

   (ledger-item 20161115 11.31 "a BANK" "a foo"  "P31" "D31"   "11" #f      #f #f)
   (ledger-item 20161120 11.32 "a foo"  "a BANK" "P32" "D32"   #f   "11"    #f #f)
   (ledger-item 20161125 11.33 "a BANK" "a foo"  "P33" "D33"   "11" #f      #f #f)

   (ledger-item 20161215 12.34 "a BANK" "a foo"  "P34" "D34"   #f   #f      #f #f)
   (ledger-item 20161220 12.35 "a foo"  "a BANK" "P35" "D35"   #f   "13"    #f #f)
   (ledger-item 20161225 12.36 "a BANK" "a foo"  "P36" "D36"   "12" #f      #f #f)

   (ledger-item 20170115 13.37 "a BANK" "a foo"  "P37" "D37"   "01" #f      #f #f)
   (ledger-item 20170120 13.38 "a foo"  "a BANK" "P38" "D38"   #f   #f      #f #f)
   (ledger-item 20170125 13.39 "a BANK" "a foo"  "P39" "D39"   #f   #f      #f #f)

   (ledger-item 20170214 14.40 "a foo"  "a BANK" "P40" "D40"   #f   #f      #f #f)
   (ledger-item 20170220 14.41 "a BANK" "a foo"  "P41" "D41"   #f   #f      #f #f)
   (ledger-item 20170226 14.42 "a foo"  "a BANK" "P42" "D42"   #f   #f      #f #f)

   (ledger-item 20170315 15.43 "a BANK" "a foo"  "P43" "D43"   #f   #f      #f #f)
   (ledger-item 20170320 15.44 "a foo"  "a BANK" "P44" "D44"   #f   #f      #f #f)
   (ledger-item 20170325 15.45 "a BANK" "a foo"  "P45" "D45"   #f   #f      #f #f)

   (ledger-item 20170414 16.46 "a foo"  "a BANK" "P46" "D46"   #f   #f      #f #f)
   (ledger-item 20170420 16.47 "a BANK" "a foo"  "P47" "D47"   #f   #f      #f #f)
   (ledger-item 20170426 16.48 "a foo"  "a BANK" "P48" "D48"   #f   #f      #f #f)

   (ledger-item 20170515 17.49 "a BANK" "a foo"  "P49" "D49"   #f   #f      #f #f)
   (ledger-item 20170520 17.50 "a foo"  "a BANK" "P50" "D50"   #f   #f      #f #f)
   (ledger-item 20170525 17.51 "a BANK" "a foo"  "P51" "D51"   #f   #f      #f #f)

   (ledger-item 20170614 18.52 "a foo"  "a BANK" "P52" "D52"   #f   #f      #f #f)
   (ledger-item 20170620 18.53 "a BANK" "a foo"  "P53" "D53"   #f   #f      #f #f)
   (ledger-item 20170626 18.54 "a foo"  "a BANK" "P54" "D54"   #f   #f      #f #f)

   (ledger-item 20170715 19.55 "a BANK" "a foo"  "P55" "D55"   #f   #f      #f #f)
   (ledger-item 20170720 19.56 "a foo"  "a BANK" "P56" "D56"   #f   #f      #f #f)
   (ledger-item 20170725 19.57 "a BANK" "a foo"  "P57" "D57"   #f   #f      #f #f)

   (ledger-item 20170815 20.58 "a BANK" "a foo"  "P58" "D58"   #f   #f      #f #f)
   (ledger-item 20170820 20.59 "a foo"  "a BANK" "P59" "D59"   #f   #f      #f #f)
   (ledger-item 20170825 20.60 "a BANK" "a foo"  "P60" "D60"   #f   #f      #f #f)

   (ledger-item 20170915 21.61 "a BANK" "a foo"  "P61" "D61"   #f   #f      #f #f)
   (ledger-item 20170920 21.62 "a foo"  "a BANK" "P62" "D62"   #f   #f      #f #f)
   (ledger-item 20170925 21.63 "a BANK" "a foo"  "P63" "D63"   #f   #f      #f #f)

   (ledger-item 20171015 22.64 "a BANK" "a foo"  "P64" "D64"   #f   #f      #f #f)
   (ledger-item 20171020 22.65 "a foo"  "a BANK" "P65" "D65"   #f   #f      #f #f)
   (ledger-item 20171025 22.66 "a BANK" "a foo"  "P66" "D66"   #f   #f      #f #f)

   (ledger-item 20171115 23.67 "a BANK" "a foo"  "P67" "D67"   #f   #f      #f #f)
   (ledger-item 20171120 23.68 "a foo"  "a BANK" "P68" "D68"   #f   #f      #f #f)
   (ledger-item 20171125 23.69 "a BANK" "a foo"  "P69" "D69"   #f   #f      #f #f)

   (ledger-item 20171215 24.70 "a BANK" "a foo"  "P70" "D70"   #f   #f      #f #f)
   (ledger-item 20171220 24.71 "a foo"  "a BANK" "P71" "D71"   #f   #f      #f #f)
   (ledger-item 20171225 24.72 "a BANK" "a foo"  "P72" "D72"   #f   #f      #f #f)))
