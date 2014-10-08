;;;; -*- Mode: emacs-lisp -*-

(("Data structures" (;; Operators
                     "Special operator" "Macro" "Local macro" "Function" "Local function" "Generic function" "Accessor"
                     ;; Variables
                     "Variable" "Constant variable"

                     ;; Type specifiers
                     "System class" "Type" "Condition type" "Class"
                     "Combining type specifier"
                     "Abbreviating type specifier"
                     "Predicating type specifier"
                     "Specializing type specifier"))
 ("Concrete"
  "cons"
  "hash-table"
  "array"
  "vector"
  "string"
  "random-state"
  "readtable"
  "sequence")
 ("Intentional"
  "lists"
  "alists"
  "plists"
  "trees"
  "sets"
  "bits"))

(("Thin wrappers" ("Macro" "Function" "Accessor"))
 "typep"
 ("Conses"
  "nth"
  "car and cdr"
  "Misc")
 "Sequences"
 "Conditionals"
 "Streams"
 "Accessor-specializing"
 "Math"
 "Conditions and restarts"
 "Misc")

(("Comparisons" ("Macro" "Function" "Accessor"))
 "Math"
 "Ordering"
 "Searching"
 "Replacing"
 "Removing"
 "Identity"
 "Structural equivalence"
 "Type equivalence")

(("Obsoletion" ("Special operator" "Macro" "Function" "Accessor" "Variable"))
 "Obsolete"
 "Discouraged"
 "Not deprecated")
