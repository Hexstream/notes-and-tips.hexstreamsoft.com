;;;; -*- Mode: lisp -*-

(("nnnnn"
  "a number")
 ("xxxxx"
  "a symbol in the current package")
 (":xxxxx"
  "a symbol in the the KEYWORD package")
 ("ppppp:xxxxx"
  "an external symbol in the ppppp package")
 ("ppppp::xxxxx"
  "a (possibly internal) symbol in the ppppp package")
 (":nnnnn"
  "undefined")
 ("ppppp:nnnnn"
  "undefined")
 ("ppppp::nnnnn"
  "undefined")
 ("::aaaaa"
  "undefined")
 ("aaaaa:"
  "undefined")
 ("aaaaa:aaaaa:aaaaa"
  "undefined"))
