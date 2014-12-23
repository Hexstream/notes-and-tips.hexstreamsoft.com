;;;; -*- Mode: lisp -*-

(("0.0"
  "Floating-point zero in default format")
 ("0E0"
  "As input, this is also floating-point zero in default format. As output, this would appear as 0.0.")
 ("0e0"
  "As input, this is also floating-point zero in default format. As output, this would appear as 0.0.")
 ("-.0"
  "As input, this might be a zero or a minus zero, depending on whether the implementation supports a distinct minus zero. As output, 0.0 is zero and -0.0 is minus zero.")
 ("0."
  "On input, the integer zero---not a floating-point number! Whether this appears as 0 or 0. on output depends on the value of *print-radix*.")
 ("0.0s0"
  "A floating-point zero in short format")
 ("0s0"
  "As input, this is a floating-point zero in short format. As output, such a zero would appear as 0.0s0 (or as 0.0 if short-float was the default format).")
 ("6.02E+23"
  "Avogadro's number, in default format")
 ("602E+21"
  "Also Avogadro's number, in default format"))
