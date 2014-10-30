;;;; -*- Mode: emacs-lisp -*-

;;; Special operators
(block ((:required name) (&body forms))
  ((&rest results)))

(return-from ((:required name) (&optional result))
  :non-local-exit)

(catch ((:required tag) (&body forms))
  ((&rest results)))

(throw ((:required tag result-form))
       :non-local-exit)

(tagbody ((&body tags-and-statements))
         ((:required (eql nil))))

(go ((:required tag))
    :non-local-exit)

(unwind-protect ((:required protected-form) (&body cleanup-forms))
  ((&rest results)))

(if ((:required test-form then-form) (&optional (else-form :default nil)))
    ((&rest results)))

(progn ((&body forms))
       ((&rest results)))

(multiple-value-prog1 ((:required first-form) (&body forms))
                      ((&rest first-form-results)))

(multiple-value-call ((:required function-form) (&body forms))
                     ((&rest results)))

(flet ((:required (:recurse
                   ((&body (:recurse
                            ((:required function-name lambda-list)
                             (&doc local-documentation)
                             (&decl local-declarations)
                             (&body local-forms)))))))
       (&decl declarations)
       (&body forms))
  ((&rest results)))

(labels ((:required (:recurse
                     ((&body (:recurse
                              ((:required function-name lambda-list)
                               (&doc local-documentation)
                               (&decl local-declarations)
                               (&body local-forms)))))))
         (&decl declarations)
         (&body forms))
  ((&rest results)))

(macrolet ((:required (:recurse
                       ((&body (:recurse
                                ((:required name lambda-list)
                                 (&doc local-documentation)
                                 (&decl local-declarations)
                                 (&body local-forms)))))))
           (&decl declarations)
           (&body forms))
  ((&rest results)))

(symbol-macrolet ((:required (:recurse
                              ((&body (:recurse
                                       ((:required symbol expansion)))))))
                  (&decl declarations)
                  (&body forms))
  ((&rest results)))

(progv ((:required symbols values) (&body forms))
    ((&rest results)))

(let ((:required (:recurse
                  ((&body (:or var (:recurse
                                    ((:required var)
                                     (&optional (init-form :default nil)))))))))
      (&decl declarations)
      (&body forms))
  ((&rest results)))

(let* ((:required (:recurse
                   ((&body (:or var (:recurse
                                     ((:required var)
                                      (&optional (init-form :default nil)))))))))
       (&decl declarations)
       (&body forms))
  ((&rest results)))

(setq ((&body (:recurse ((var form)))))
      ((:required result)))

(eval-when ((:required (:recurse ((&rest situations))))
            (&body forms))
  ((&rest results)))

(load-time-value ((:required form) (&optional read-only-p))
                 ((:required object)))

(quote ((:required object))
       ((:required object)))

(function ((:required name))
          ((:required function)))

(locally ((&decl declarations) (&body forms))
         ((&rest results)))

(the ((:required value-type form))
  ((&rest results)))


;;; Macros

(cond ((&body (:required (:recurse
                          ((:required test-form)
                           (&body forms))))))
      ((:rest results)))

(and ((&body forms))
     ((:rest results)))

(or ((&body forms))
    ((:rest results)))

(when ((:required test-form)
       (&body forms))
    ((:rest results)))

(unless ((:required test-form)
         (&body forms))
  ((:rest results)))

;; Can't represent otherwise clause accurately for now.
(case ((:required keyform)
       (&body (:recurse
               ((:required keys)
                (&body forms)))))
  ((:rest results)))

(ccase ((:required keyplace)
       (&body (:recurse
               ((:required keys)
                (&body forms)))))
  ((:rest results)))

(ecase ((:required keyform)
        (&body (:recurse
                ((:required keys)
                 (&body forms)))))
  ((:rest results)))

;; Can't represent otherwise clause accurately for now.
(typecase ((:required keyform)
       (&body (:recurse
               ((:required keys)
                (&body forms)))))
  ((:rest results)))

(ctypecase ((:required keyplace)
       (&body (:recurse
               ((:required keys)
                (&body forms)))))
  ((:rest results)))

(etypecase ((:required keyform)
            (&body (:recurse
                    ((:required keys)
                     (&body forms)))))
  ((:rest results)))

(defclass ((:required class-name
                      (:recurse
                       ((&rest superclass-names)))
                      (:recurse
                       ((&body (:or slot-name
                                    (&key (((:reader reader-function-name))
                                           ((:writer writer-function-name))
                                           ((:accessor writer-function-name))
                                           ((:allocation allocation-type))
                                           ((:initarg initarg-name))
                                           ((:initform form))
                                           ((:type type-specifier))
                                           ((:documentation string)))))))))
           ???)
  ((:required new-class)))

(defstruct ???
  ((:required structure-name)))

(define-condition ???
  ((:required name)))

(define-setf-expander ((:required access-fn lambda-list)
                       (&doc documentation)
                       (&decl declarations)
                       (&body forms))
  ((:required access-fn)))

(define-modify-macro ((:required name lambda-list function)
                      (&optional documentation))
  ((:required name)))

(defsetf (:or ((:required access-fn update-fn)
               (&optional documentation))
              ((:required access-fn
                          lambda-list
                          (:recurse ((&rest store-variables))))
               (&doc documentation)
               (&decl declarations)
               (&body forms)))
  ((:required access-fn)))

(defmacro ((:required name lambda-list)
           (&doc documentation)
           (&decl declarations)
           (&body forms))
  ((:required name)))

(define-symbol-macro ((:required symbol expansion))
  ((:required symbol)))

(define-compiler-macro ((:required name lambda-list)
                        (&doc documentation)
                        (&decl declarations)
                        (&body forms))
  ((:required name)))

(deftype ((:required name lambda-list)
          (&doc documentation)
          (&decl declarations)
          (&body forms))
  ((:required name)))

(defpackage ???
  ((:required package)))

(defun ((:required function-name lambda-list)
        (&doc documentation)
        (&decl declarations)
        (&body forms)))
  ((:required function-name)))

(defgeneric ???
  ((:required new-generic)))

(defmethod ???
  ((:required new-method)))

(define-method-combination ???
  ((:required name)))

(defvar ((:required name)
         (&optional initial-value documentation))
  ((:required name)))

(defparameter ((:required name initial-value)
               (&optional documentation))
  ((:required name)))

(defconstant ((:required name initial-value)
               (&optional documentation))
  ((:required name)))

(do ((:required (:recurse
                 ((&body (:or var
                              (:recurse
                               ((:required var)
                                (&optional init-form step-form)))))))
                (:recurse
                 ((:required end-test-form)
                  (&body result-forms))))
     (&decl declarations)
     (&body tags-and-statements))
    ((:required results)))

(do* ((:required (:recurse
                  ((&body (:or var
                               (:recurse
                                ((:required var)
                                 (&optional init-form step-form)))))))
                 (:recurse
                  ((:required end-test-form)
                   (&body result-forms))))
      (&decl declarations)
      (&body tags-and-statements))
    ((:required results)))

(do-symbols ((:required (:recurse
                         ((:required var)
                          (&optional package (result-form :default nil)))))
             (&decl declarations)
             (&body tags-and-statements))
  ((:required results)))

(do-external-symbols ((:required (:recurse
                                  ((:required var)
                                   (&optional package (result-form :default nil)))))
                      (&decl declarations)
                      (&body tags-and-statements))
                     ((:required results)))

(do-all-symbols ((:required (:recurse
                             ((:required var)
                              (&optional package (result-form :default nil)))))
                 (&decl declarations)
                 (&body tags-and-statements))
  ((:required results)))

(dolist ((:required (:recurse
                     ((:required var list-form)
                      (&optional (result-form :default nil)))))
         (&decl declarations)
         (&body tags-and-statements))
  ((&rest results)))

(dotimes ((:required (:recurse
                      ((:required var count-form)
                       (&optional (result-form :default nil)))))
          (&decl declarations)
          (&body tags-and-statements))
  ((&rest results)))

;; TODO: At least a bit more explicit.
(loop ((&body keywords-and-forms))
      ((&rest results)))

(loop-finish ()
             :non-local-exit)

(pprint-logical-block ((:required (:recurse
                                   ((:required stream-symbol object)
                                    (&key prefix
                                          per-line-prefix
                                          (suffix :default "")))))
                       (&decl declarations)
                       (&body forms))
                      ((:required (eql nil))))

(pprint-exit-if-list-exhausted ()
                               ((:required (eql nil))))

(pprint-pop ()
            ((:required object)))

(with-hash-table-iterator ((:required (:recurse
                                       ((:required name hash-table))))
                           (&decl declarations)
                           (&body forms))
                          ((&rest results)))

(with-package-iterator ((:required (:recurse
                                    ((:required name package-list-form)
                                     (&rest symbol-types))))
                        (&decl declarations)
                        (&body forms))
                       ((&rest results)))

(setf ((&rest places-and-values))
      ((&rest results)))

(psetf ((&rest places-and-values))
       ((:required (eql nil))))

(psetq ((&rest vars-and-forms))
       ((:required (eql nil))))

(multiple-value-setq ((:required vars forms))
  ((:required result)))

(rotatef ((&rest places))
         ((:required (eql nil))))

;; Can't represent accurately for now.
(shiftf ((&rest args))
        ((:required old-value-1)))

(incf ((:required place)
       (&optional (delta-form :default 1)))
      ((:required new-value)))

(decf ((:required place)
       (&optional (delta-form :default 1)))
      ((:required new-value)))

(push ((:required item place))
      ((:required new-place-value)))

(pushnew ((:required item place)
          (&key (key :default #'identity) (test :default #'eql) test-not))
         ((:required new-place-value)))

(pop ((:required place))
     ((:required element)))

(remf ((:required place indicator))
      ((:required generalized-boolean)))

(in-package ((:required name))
            ((:required package)))

(assert ((:required test-form)
         (&optional (:recurse
                     ((&rest places)))
                    datum-form)
         (&rest argument-forms))
        ((:required (eql nil))))

(check-type ((:required place typespec)
             (&optional string))
            ((&rest (eql nil))))

(handler-bind ((:required (:recurse
                           ((&rest (:recurse
                                    ((:required type handler)))))))
               (&body forms))
              ((&rest results)))

;; Can't represent fully accurately for now (but almost).
;; Missing detail is that no-error-clause can appear only once and must be last.
(handler-case ((:required expression)
               (&body (:recurse (:or ((:required typespec
                                                 (:recurse
                                                  ((&optional var))))
                                      (&decl declarations)
                                      (&body forms))
                                     ((:required (eql :no-error) lambda-list)
                                      (&decl declarations)
                                      (&body forms))))))
              ((&rest results)))

(ignore-errors ((&body forms))
               ((&rest results)))

;; Can't accurately represent that each &key probably must be supplied once at most.
(restart-bind ((&rest (:recurse
                       ((:required name function)
                        (&key interactive-function
                              report-function
                              test-function)))))
              ((&rest results)))

;; Can't accurately represent that each &key probably must be supplied once at most.
(restart-case ((:required restartable-form)
               (&body (:recurse
                       ((:required case-name lambda-list)
                        (&key ((:interactive interactive-expression)
                               (:report report-expression)
                               (:test test-expression)))
                        (&decl declarations)
                        (&body body)))))
              ((&rest results)))

(with-compilation-unit ((:required (:recurse
                                    ((&key (override :default nil)))))
                        (&body forms))
                       ((&rest results)))

(with-condition-restarts ((:required condition-form restarts-form)
                          (&body forms))
                         ((&rest results)))

(with-simple-restart ((:required (:recurse
                                  ((:required name format-control)
                                   (&rest format-arguments))))
                      (&body forms))
                     ((&rest results)))

(declaim ((&rest declaration-specifiers))
         :implementation-dependent)

(destructuring-bind ((:required lambda-list expression)
                     (&decl declarations)
                     (&body forms))
    ((&rest results)))

(lambda ((:required lambda-list)
         (&doc documentation)
         (&decl declarations)
         (&body forms))
  ((:required function)))

(make-method ((:required form))
             ((:required method-object)))

(call-method ((:required method)
              (&optional next-method-list))
             ((&rest results)))

(multiple-value-bind ((:required (:recurse (&rest vars))
                                 values-form)
                      (&decl declarations)
                      (&body forms))
    ((&rest results)))

(multiple-value-list ((:required form))
                     ((:required list)))

(nth-value ((:required n form))
           ((:required object)))

(print-unreadable-object ((:required (:recurse
                                      ((:required object stream)
                                       (&key (type :default nil) (identity :default nil)))))
                          (&body forms))
                         ((:required (eql nil))))

(prog ((:required (:recurse
                   ((&rest (:or var
                                (:recurse
                                 ((:required var)
                                  (&optional (init-form :default nil)))))))))
       (&decl declarations)
       (&body tags-and-statements))
      ((&rest results)))

(prog* ((:required (:recurse
                    ((&rest (:or var
                                 (:recurse
                                  ((:required var)
                                   (&optional (init-form :default nil)))))))))
        (&decl declarations)
        (&body tags-and-statements))
       ((&rest results)))

(prog1 ((:required first-form)
        (&body forms))
  ((:required result-1)))

(prog2 ((:required first-form second-form)
        (&body forms))
  ((:required result-2)))

(return ((:required result))
        :non-local-exit)

(step ((:required form))
      ((&rest results)))

(time ((:required form))
      ((&rest results)))

;; trace-result is implementation-dependent unless no function-names are supplied.
(trace ((&rest function-names))
       ((:required trace-result)))

;; untrace-result is implementation-dependent.
(untrace ((&rest function-names))
         ((:required untrace-result)))

(with-accessors ((:required (:recurse
                             ((&rest (:recurse
                                      ((:required variable-name
                                                  accessor-name))))))
                            instance-form)
                 (&decl declarations)
                 (&body forms))
                ((&rest results)))

(with-slots ((:required (:recurse
                         ((&rest (:or slot-name
                                      (:recurse
                                       ((:required variable-name
                                                   slot-name)))))))
                        instance-form)
             (&decl declarations)
             (&body forms))
    ((&rest results)))

(with-input-from-string ((:required (:recurse
                                     ((:required var string)
                                      (&key index
                                            (start :default 0)
                                            (end :default nil)))))
                         (&decl declarations)
                         (&body forms))
                        ((&rest results)))

(with-output-to-string ((:required (:recurse
                                    ((:required var)
                                     (&optional (string-form :default nil))
                                     (&key (element-type :default 'character)))))
                        (&decl declarations)
                        (&body forms))
                       ((&rest results)))

(with-open-file ((:required (:recurse
                             ((:required stream filespec)
                              (&rest options))))
                 (&decl declarations)
                 (&body forms))
                ((&rest results)))

(with-open-stream ((:required (:recurse
                               ((:required var stream)
                                (&decl declarations)
                                (&body forms)))))
                  ((&rest results)))

(with-standard-io-syntax ((&body forms))
                         ((&rest results)))

(special-operator-p ((:required symbol))
                    ((:required generalized-boolean)))

(macro-function ((:required symbol)
                 (&optional environment))
                ((:required function))
                (:store-vars new-function))

(compiler-macro-function ((:required name)
                          (&optional environment))
                         ((:required function))
                         (:store-vars new-function))

(symbol-value ((:required symbol))
              ((:required value))
              (:store-vars new-value))

(symbol-function ((:required symbol))
                 ((:required contents))
                 (:store-vars new-contents))

(fdefinition ((:required function-name))
             ((:required definition))
             (:store-vars new-definition))

(makunbound ((:required symbol))
            ((:required symbol)))

(fmakunbound ((:required name))
             ((:required name)))

(ceiling ((:required number)
          (&optional (divisor :default 1)))
         ((:required quotient remainder)))

(fceiling ((:required number)
           (&optional (divisor :default 1)))
          ((:required quotient remainder)))

(floor ((:required number)
        (&optional (divisor :default 1)))
       ((:required quotient remainder)))

(ffloor ((:required number)
         (&optional (divisor :default 1)))
        ((:required quotient remainder)))

(round ((:required number)
        (&optional (divisor :default 1)))
       ((:required quotient remainder)))

(fround ((:required number)
         (&optional (divisor :default 1)))
        ((:required quotient remainder)))

(truncate ((:required number)
           (&optional (divisor :default 1)))
          ((:required quotient remainder)))

(ftruncate ((:required number)
            (&optional (divisor :default 1)))
           ((:required quotient remainder)))

(sin ((:required radians))
     ((:required number)))

(cos ((:required radians))
     ((:required number)))

(tan ((:required radians))
     ((:required number)))

(sinh ((:required number))
      ((:required result)))

(asin ((:required number))
      ((:required radians)))

(cosh ((:required number))
      ((:required result)))

(acos ((:required number))
      ((:required radians)))

(tanh ((:required number))
      ((:required result)))

(atan ((:required number1)
       (&optional number2))
      ((:required radians)))

(asinh ((:required number))
       ((:required result)))

(acosh ((:required number))
       ((:required result)))

(atanh ((:required number))
       ((:required result)))

(ash ((:required integer count))
     ((:required shifted-integer)))

(boole ((:required op integer-1 integer-2))
       ((:required result-integer)))

(byte ((:required size position))
      ((:required bytespec)))

(byte-position ((:required bytespec))
               ((:required position)))

(byte-size ((:required bytespec))
           ((:required size)))

(deposit-field ((:required newbyte bytespec integer))
               ((:required result-integer)))

(dpb ((:required newbyte bytespec integer))
     ((:required result-integer)))

(ldb ((:required bytespec integer))
     ((:required byte))
     (:store-vars new-byte))

(ldb-test ((:required bytespec integer))
          ((:required generalized-boolean)))

(integer-length ((:required integer))
                ((:required number-of-bits)))

(logand ((&rest integers))
        ((:required result-integer)))

(logandc1 ((:required integer-1 integer-2))
          ((:required result-integer)))

(logandc2 ((:required integer-1 integer-2))
          ((:required result-integer)))

(logbitp ((:required index integer))
         ((:required generalized-boolean)))

(logcount ((:required integer))
          ((:required number-of-on-bits)))

(logeqv ((&rest integers))
        ((:required result-integer)))

(logior ((&rest integers))
        ((:required result-integer)))

(lognand ((:required integer-1 integer-2))
         ((:required result-integer)))

(lognor ((:required integer-1 integer-2))
        ((:required result-integer)))

(lognot ((:required integer))
        ((:required result-integer)))

(logorc1 ((:required integer-1 integer-2))
         ((:required result-integer)))

(logorc2 ((:required integer-1 integer-2))
         ((:required result-integer)))

(logtest ((:required integer-1 integer-2))
         ((:required generalized-boolean)))

(logxor ((&rest integers))
        ((:required result-integer)))

;; Can't represent that second argument is called "place" in setf version.
(mask-field ((:required bytespec integer))
            ((:required masked-integer))
            (:store-vars new-masked-integer))

(+ ((&rest numbers))
   ((:required sum)))

;; Can't represent accurately.
(- (:or ((:required number))
        ((:required minuend)
         (&rest+ subtrahends)))
   (:or ((:required negation))
        ((:required difference))))

(* ((&rest numbers))
   ((:required product)))

;; Can't represent accurately.
(/ (:or ((:required number))
        ((:required numerator)
         (&rest+ denominators)))
   (:or ((:required reciprocal))
        ((:required quotient))))

((:or < <= /= = >= >) ((&rest+ numbers))
 ((:required generalized-boolean)))

(1+ ((:required number))
    ((:required successor)))

(1+ ((:required number))
    ((:required predecessor)))

(abs ((:required number))
     ((:required absolute-value)))

(arithmetic-error-operation ((:required condition))
                            ((:required operation)))

(arithmetic-error-operands ((:required condition))
                           ((:required operands)))

(cis ((:required radians))
     ((:required number)))

(complex ((:required realpart)
          (&optional imagpart))
         ((:required complex)))

(realpart ((:required number))
          ((:required real)))

(imagpart ((:required number))
          ((:required real)))

(complexp ((:required object))
          ((:required generalized-boolean)))

(conjugate ((:required number))
           ((:required conjugate)))

(decode-float ((:required float))
              ((:required significand exponent sign)))

(scale-float ((:required float integer))
             ((:required scaled-float)))

(float-radix ((:required float))
             ((:required float-radix)))

(float-sign ((:required float-1)
             (&optional float-2))
            ((:required signed-float)))

;; Numeral only used to differentiate args since they're on same page...
(float-digits ((:required float))
              ((:required digits1)))

(float-precision ((:required float))
                 ((:required digits2)))

(integer-decode-float ((:required float))
                      ((:required significand exponent integer-sign)))

(numerator ((:required rational))
           ((:required numerator)))

(denominator ((:required rational))
             ((:required denominator)))

((:or evenp oddp) ((:required integer))
 ((:required generalized-boolean)))

(expt ((:required base-number power-number))
      ((:required result)))

(exp ((:required number))
     ((:required result)))

(float ((:required number)
        (&optional prototype))
       ((:required float)))

(floatp ((:required object))
        ((:required generalized-boolean)))

(gcd ((&rest integers))
     ((:required greatest-common-denominator)))

(lcm ((&rest integers))
     ((:required least-common-multiple)))

(integerp ((:required object))
          ((:required generalized-boolean)))

(log ((:required number)
      (&optional base))
     ((:required logarithm)))

(make-random-state ((&optional (state :default nil)))
                   ((:required new-state)))

(min ((&rest+ reals))
     ((:required min-real)))

(max ((&rest+ reals))
     ((:required max-real)))

(mod ((:required number divisor))
     ((:required modulus)))

(rem ((:required number divisor))
     ((:required remainder)))

(numberp ((:required object))
         ((:required generalized-boolean)))

(parse-integer ((:required string)
                (&key (start :default 0)
                      (end :default nil)
                      (radix :default 10)
                      (junk-allowed :default nil)))
               ((:required integer pos)))

(phase ((:required number))
       ((:required phase)))

(random ((:required limit)
         (&optional (random-state :default *random-state*)))
        ((:required random-number)))

(random-state-p ((:required object))
                ((:required generalized-boolean)))

((:or rational rationalize) ((:required number))
 ((:required rational)))

((:or rationalp realp) ((:required object))
 ((:required generalized-boolean)))

(sqrt ((:required number))
      ((:required root)))

(isqrt ((:required natural))
       ((:required natural-root)))

(signum ((:required number))
        ((:required signed-prototype)))

(minusp ((:required real))
        ((:required generalized-boolean)))

(zerop ((:required number))
       ((:required generalized-boolean)))

(plusp ((:required real))
       ((:required generalized-boolean)))

(gethash ((:required key hash-table)
          (&optional (default :default nil)))
         ((:required value present-p))
         (:store-vars new-value))

(remhash ((:required key hash-table))
         ((:required generalized-boolean)))

(clrhash ((:required hash-table))
         ((:required hash-table)))

(make-hash-table ((&key (test :default 'eql)
                        (size :default :implementation-dependent)
                        (rehash-size :default :implementation-dependent)
                        (rehash-threshold :default :implementation-dependent)))
                 ((:required hash-table)))

(hash-table-count ((:required hash-table))
                  ((:required count)))

(hash-table-size ((:required hash-table))
                  ((:required size)))

(hash-table-p ((:required object))
              ((:required generalized-boolean)))

(hash-table-test ((:required hash-table))
                 ((:required test)))

(hash-table-rehash-size ((:required hash-table))
                        ((:required rehash-size)))

(hash-table-rehash-threshold ((:required hash-table))
                             ((:required rehash-threshold)))

(maphash ((:required function hash-table))
         ((:required (eql nil))))

(sxhash ((:required object))
        ((:required hash-code)))

(fill-pointer ((:required vector))
              ((:required fill-pointer))
              (:store-vars new-fill-pointer))

(simple-vector-p ((:required object))
                 ((:required generalized-boolean)))

(svref ((:required simple-vector index))
       ((:required element))
       (:store-vars new-element))

(vector ((&rest objects))
        ((:required vector)))

(vector-push ((:required new-element vector))
             ((:required new-index-p)))

(vector-extend ((:required new-element vector)
                (&optional (extension :default :implementation-dependent)))
               ((:required new-index)))

(vector-pop ((:required vector))
            ((:required element)))

(vectorp ((:required object))
         ((:required generalized-boolean)))

(string-downcase ((:required string)
                  (&key (start :default 0)
                        (end :default nil)))
                 (:required cased-string))

(nstring-downcase ((:required string)
                   (&key (start :default 0)
                         (end :default nil)))
                  (:required string))

(string-upcase ((:required string)
                (&key (start :default 0)
                      (end :default nil)))
               (:required cased-string))

(nstring-upcase ((:required string)
                 (&key (start :default 0)
                       (end :default nil)))
                (:required string))

(string-capitalize ((:required string)
                    (&key (start :default 0)
                          (end :default nil)))
                   (:required cased-string))

(nstring-capitalize ((:required string)
                     (&key (start :default 0)
                           (end :default nil)))
                    (:required string))

((or char schar) ((:required string index))
 ((:required character))
 (:store-vars new-character))

(make-string ((:required size)
              (&key (initial-element :default :implementation-dependent)
                    (element-type :default 'character)))
             ((:required string)))

(simple-string-p ((:required object))
                 ((:required generalized-boolean)))

(string ((:required x))
        ((:required string)))

;; FIXME: Order divergence.
((or string< string<= string/= string>= string>
     string-lessp string-not-greaterp string-not-equal string-not-lessp string-greaterp)
 ((:required string1 string2)
  (&key (start1 :default 0)
        (end1 :default nil)
        (start2 :default 0)
        (end2 :default nil)))
 ((:required mismatch-index)))

((or string= string-equal)
 ((:required string1 string2)
  (&key (start1 :default 0)
        (end1 :default nil)
        (start2 :default 0)
        (end2 :default nil)))
 ((:required generalized-boolean)))

((or string-trim string-left-trim string-right-trim)
 ((:required character-bag string))
 ((:required trimmed-string)))

(stringp ((:required object))
         ((:required generalized-boolean)))

((or bit sbit)
 ((:required bit-array)
  (&rest subscripts))
 ((:required bit))
 (:store-vars new-bit))

;; FIXME: Order divergence.
((or bit-and bit-andc1 bit-andc2 bit-eqv bit-ior bit-nand bit-nor bit-orc1 bit-orc2 bit-xor)
 ((:required bit-array1 bit-array2)
  (&optional (opt-arg :default nil)))
 ((:required resulting-bit-array)))

(bit-not
 ((:required bit-array)
  (&optional (opt-arg :default nil)))
 ((:required resulting-bit-array)))

((or bit-vector-p simple-bit-vector-p)
 ((:required object))
 ((:required generalized-boolean)))

;; TODO: Maybe add some defaults but it's a bit complex here.
(adjust-array ((:required array new-dimensions)
               (&key element-type
                     initial-element
                     initial-contents
                     fill-pointer
                     displaced-to
                     displaced-index-offset))
              ((:required adjusted-array)))

(adjustable-array-p ((:required array))
                    ((:required generalized-boolean)))

(aref ((:required array)
       (&rest subscripts))
      ((:required element))
      (:store-vars new-element))

(row-major-aref ((:required array index))
                ((:required element))
                (:store-vars new-element))

(array-dimension ((:required array axis-number))
                 ((:required dimension)))

(array-dimensions ((:required array))
                  ((:required dimensions)))

(array-displacement ((:required array))
                    ((:required displaced-to displaced-index-offset)))

(array-element-type ((:required array))
                    ((:required typespec)))

(array-has-fill-pointer-p ((:required array))
                          ((:required generalized-boolean)))

(array-in-bounds-p ((:required array)
                    (&rest subscripts))
                   ((:required generalized-boolean)))

(array-rank ((:required array))
            ((:required rank)))

(array-row-major-index ((:required array)
                        (&rest subscripts))
                       ((:required index)))

(array-total-size ((:required array))
                  ((:required size)))

(arrayp ((:required object))
        ((:required generalized-boolean)))

(make-array ((:required dimensions)
             (&rest (element-type t)
                    (initial-element :default implementation-dependent)
                    initial-contents
                    (adjustable :default nil)
                    (fill-pointer :default nil)
                    (displaced-to :default nil)
                    (displaced-index-offset :default 0)))
            ((:required new-array)))

(upgraded-array-element-type ((:required typespec)
                              (&optional environment))
                             ((:required upgraded-typespec)))

(close ((:required stream)
        (&key (abort :default nil)))
       (:required result))

((or input-stream-p output-stream-p)
 ((:required stream))
 ((:required generalized-boolean)))

(interactive-stream-p ((:required stream))
                      ((:required generalized-boolean)))

((or y-or-n-p yes-or-no-p)
 ((&optional control)
  (&rest arguments))
 ((:required generalized-boolean)))

(make-broadcast-stream ((&rest streams))
                       ((:required broadcast-stream)))

(broadcast-stream-streams ((:required broadcast-stream))
                          ((:required streams)))

(make-concatenated-stream ((&rest input-streams))
                          ((:required concatenated-stream)))

(concatenated-stream-streams ((:required concatenated-stream))
                             ((:required streams)))

(make-echo-stream ((:required input-stream output-stream))
                  ((:required echo-stream)))

(echo-stream-input-stream ((:required echo-stream))
                          ((:required input-stream)))

(echo-stream-output-stream ((:required echo-stream))
                           ((:required output-stream)))

(make-string-input-stream ((:required string)
                           (&optional (start :default 0)
                                      (end :default nil))
                           ((:required string-stream))))

(make-string-output-stream ((&key (element-type :default 'character)))
                           ((:required string-stream)))

(get-output-stream-string ((:required string-output-stream))
                          ((:required string)))

(make-synonym-stream ((:required symbol))
                     ((:required synonym-stream)))

(synonym-stream-symbol ((:required synonym-stream))
                       ((:required symbol)))

(make-two-way-stream ((:required input-stream output-stream))
                     ((:required two-way-stream)))

(two-way-stream-input-stream ((:required two-way-stream))
                             ((:required input-stream)))

(two-way-stream-output-stream ((:required two-way-stream))
                              ((:required output-stream)))

(open-stream-p ((:required stream))
               ((:required generalized-boolean)))

(stream-element-type ((:required stream))
                     ((:required typespec)))

(stream-external-format ((:required stream))
                        ((:required format)))

(stream-error-stream ((:required condition))
                     ((:required stream)))

(streamp ((:required object))
         ((:required generalized-boolean)))

(readtablep ((:required object))
            ((:required generalized-boolean)))

(readtable-case ((:required readtable))
                ((:required mode))
                (:store-vars mode))

(copy-readtable ((&optional (from-readtable :default *readtable*)
                            (to-readtable :default nil)))
                ((:required readtable)))

(get-macro-character ((:required char)
                      (&optional (readtable :default *readtable*)))
                     ((:required function non-terminating-p)))

(set-macro-character ((:required char new-function)
                      (&optional (non-terminating-p :default nil)
                                 (readtable :default *readtable*)))
                     ((:required (eql t))))

(make-dispatch-macro-character ((:required char)
                                (&optional (non-terminating-p :default nil)
                                           (readtable :default *readtable*)))
                               ((:required (eql t))))

(get-dispatch-macro-character ((:required disp-char sub-char)
                               (&optional (readtable :default *readtable*)))
                              ((:required function)))

(set-dispatch-macro-character ((:required disp-char sub-char new-function)
                               (&optional (readtable :default *readtable*)))
                              ((:required (eql t))))

(set-syntax-from-char ((:required to-char from-char)
                       (&optional (to-readtable :default *readtable*)
                                  from-readtable))
                      ((:required (eql t))))

(clear-input ((&optional (input-stream :default *standard-input*)))
             ((:required (eql nil))))

(listen ((&optional (input-stream :default *standard-input*)))
        ((:required generalized-boolean)))

(peek-char ((&optional (peek-type :default nil)
                       (input-stream :default *standard-input*)
                       (eof-error-p :default t)
                       (eof-value :default nil)
                       (recursive-p :default nil)))
           ((:required char)))

(read ((&optional (input-stream :default *standard-input*)
                  (eof-error-p :default t)
                  (eof-value :default nil)
                  (recursive-p :default nil)))
      ((:required object)))

(read-byte ((:required stream)
            (&optional (eof-error-p :default t)
                       (eof-value :default nil)))
           ((:required byte)))

((or read-char read-char-no-hang)
 ((&optional (input-stream :default *standard-input*)
             (eof-error-p :default t)
             (eof-value :default nil)
             (recursive-p :default nil)))
 ((:required char)))

(read-delimited-list ((:required char)
                      (&optional (input-stream :default *standard-input*)
                                 (recursive-p :default nil)))
                     (:required list))

(read-from-string ((:required string)
                   (&optional (eof-error-p :default t)
                              (eof-value :default nil))
                   (&key (start :default 0)
                         (end :default nil)
                         (preserve-whitespace :default nil)))
                  ((:required object position)))

(read-line (&optional (input-stream :default *standard-input*)
                      (eof-error-p :default t)
                      (eof-value :default nil)
                      (recursive-p :default nil))
           ((:required line missing-newline-p)))

(read-preserving-whitespace ((&optional (input-stream :default *standard-input*)
                                        (eof-error-p :default t)
                                        (eof-value :default nil)
                                        (recursive-p :default nil)))
                            ((:required object)))

(read-sequence ((:required sequence stream)
                (&key (start :default 0)
                      (end :default nil)))
               ((:required position)))

(unread-char ((:required character)
              (&optional (input-stream :default *standard-input*)))
             ((:required (eql nil))))

(copy-pprint-dispatch ((&optional (table :default *print-pprint-dispatch*)))
                      ((:required new-table)))

((or finish-output force-output clear-output)
 ((&optional (output-stream :default *standard-output*)))
 ((:required (eql nil))))

(format ((:required destination control-string)
         (&rest args))
        ((:required result)))

(formatter ((:required control-string))
           ((:required function)))

(pprint-dispatch ((:required object)
                  (&optional (table :default *print-pprint-dispatch*)))
                 ((:required function found-p)))

((or pprint-fill pprint-linear)
 ((:required stream object)
  (&optional (colon-p :default t)
             (at-sign-p :default :implementation-dependent)))
 ((:required (eql nil))))

(pprint-tabular
 ((:required stream object)
  (&optional (colon-p :default t)
             (at-sign-p :default :implementation-dependent)
             (tabsize :default 16)))
 ((:required (eql nil))))

(pprint-newline ((:required kind)
                 (&optional (stream :default *standard-output*)))
                ((:required (eql nil))))

(pprint-indent ((:required relative-to n)
                (&optional (stream :default *standard-output*)))
               ((:required (eql nil))))

(pprint-tab ((:required kind colnum colinc)
             (&optional (stream :default *standard-output*)))
            ((:required (eql nil))))

;; FIXME: Order divergence.
((or prin1 princ print)
 ((:required object)
  (&optional (output-stream :default *standard-output*)))
 ((:required object)))

(pprint
 ((:required object)
  (&optional (output-stream :default *standard-output*)))
 ())

((or prin1-to-string princ-to-string)
 ((:required object))
 ((:required string)))

(print-object ((:required object stream))
              ((:required object)))

(print-not-readable-object ((:required condition))
                           ((:required object)))

(set-pprint-dispatch ((:required type-specifier function)
                      (&optional (priority :default 0)
                                 (table :default *print-pprint-dispatch*)))
                     ((:required (eql nil))))

(write ((:required object)
        (&key (array :default *print-array*)
              (base :default *print-base*)
              (case :default *print-case*)
              (circle :default *print-circle*)
              (escape :default *print-escape*)
              (gensym :default *print-gensym*)
              (length :default *print-length*)
              (level :default *print-level*)
              (lines :default *print-lines*)
              (miser-width :default *print-miser-width*)
              (pprint-dispatch :default *print-pprint-dispatch*)
              (pretty :default *print-pretty*)
              (radix :default *print-radix*)
              (readably :default *print-readably*)
              (right-margin :default *print-right-margin*)
              (stream :default *standard-output*)))
       ((:required object)))

(write-to-string ((:required object)
                  (&key (array :default *print-array*)
                        (base :default *print-base*)
                        (case :default *print-case*)
                        (circle :default *print-circle*)
                        (escape :default *print-escape*)
                        (gensym :default *print-gensym*)
                        (length :default *print-length*)
                        (level :default *print-level*)
                        (lines :default *print-lines*)
                        (miser-width :default *print-miser-width*)
                        (pprint-dispatch :default *print-pprint-dispatch*)
                        (pretty :default *print-pretty*)
                        (radix :default *print-radix*)
                        (readably :default *print-readably*)
                        (right-margin :default *print-right-margin*)))
                 ((:required string)))

(write-byte ((:required byte stream))
            ((:required byte)))

(write-char ((:required character)
             (&optional (output-stream :default *standard-output*)))
            ((:required character)))

(write-sequence ((:required sequence stream)
                 (&key (start :default 0)
                       (end :default nil)))
                ((:required sequence)))

((or write-string write-line)
 ((:required string)
  (&optional (output-stream :default *standard-output*))
  (&key (start :default 0)
        (end :default nil)))
 ((:required string)))

(terpri ((&optional (output-stream :default *standard-output*)))
        ((:required (eql nil))))

(fresh-line ((&optional (output-stream :default *standard-output*)))
            ((:required generalized-boolean)))

(compile-file ((:required input-file)
               (&key (output-file :default :implementation-defined)
                     (verbose :default *compile-verbose*)
                     (print :default *compile-print*)
                     (external-format :default :default)))
              ((:required output-truename warnings-p failure-p)))

(compile-file-pathname ((:required input-file)
                        (&key+ (output-file :default :implementation-defined)))
                       ((:required pathname)))

(directory ((:required pathspec)
            (&key))
           ((:required pathnames)))

(delete-file ((:required filespec))
             ((:required (eql t))))

(ensure-directories-exist ((:required pathspec)
                           (&key verbose))
                          ((:required pathspec created)))

(file-author ((:required pathspec))
             ((:required author)))

(file-error-pathname ((:required condition))
                     ((:required pathspec)))

(file-length ((:required stream))
             ((:required length)))

(file-namestring ((:required pathname))
                 ((:required namestring)))

(file-position (:or ((:required stream))
                    ((:required stream position-spec)))
               (:or ((:required position))
                    ((:required success-p))))

(file-string-length ((:required stream object))
                    ((:required length)))

(file-write-date ((:required pathspec))
                 ((:required date)))

(open ((:required filespec)
       (&key (direction :default :input)
             (element-type :default 'character)
             (if-exists :default (if (eq (pathname-version filespec) :newest)
                                     :new-version
                                   :error))
             if-does-not-exist ; FIXME: Complex default.
             (external-format :default :default)))
      ((:required stream)))

(probe-file ((:required pathspec))
            ((:required truename)))

(rename-file ((:required filespec new-name))
             ((:required defaulted-new-name old-truename new-truename)))

(truename ((:required filespec))
          ((:required truename)))

(user-homedir-pathname ((&optional (host :default :implementation-dependent)))
                       ((:required pathname)))

(enough-namestring ((:required pathname)
                    (&optional (defaults :default *default-pathname-defaults*)))
                   ((:required namestring)))

((or host-namestring directory-namestring)
 ((:required pathname))
 ((:required namestring)))

(logical-pathname ((:required pathspec))
                  ((:required logical-pathname)))

(logical-pathname-translations ((:required host))
                               ((:required tranlations))
                               (:store-vars new-translations))

(load-logical-pathname-translations ((:required host))
                                    ((:required just-loaded)))

(make-pathname ((&key host device directory name type version defaults case))
               ((:required pathname)))

(merge-pathname ((:required pathname)
                 (&optional (default-pathname :default *default-pathname-defaults*)
                            (default-version :default :newest)))
                ((:required merged-pathname)))

(namestring ((:required pathname))
            ((:required namestring)))

(parse-namestring ((:required thing)
                   (&optional host
                              (default-pathname :default *default-pathname-defaults*))
                   (&key (start :default 0)
                         (end :default nil)
                         (junk-allowed :default nil)))
                  ((:required pathname position)))

(pathname ((:required pathspec))
          ((:required pathname)))

(pathname-host ((:required pathname)
                (&key (case :default :local)))
               ((:required host)))

(pathname-device ((:required pathname)
                  (&key (case :default :local)))
                 ((:required device)))

(pathname-directory ((:required pathname)
                     (&key (case :default :local)))
                    ((:required directory)))

(pathname-name ((:required pathname)
                (&key (case :default :local)))
               ((:required name)))

(pathname-type ((:required pathname)
                (&key (case :default :local)))
               ((:required type)))

(pathname-version ((:required pathname))
                  ((:required version)))

(pathname-match-p ((:required pathname wildcard))
                  ((:required generalized-boolean)))

(pathnamep ((:required object))
           ((:required generalized-boolean)))

(translate-logical-pathname ((:required pathname)
                             (&key))
                            ((:required physical-pathname)))

(translate-pathname ((:required source from-wildcard to-wildcard)
                     (&key))
                    ((:required translated-pathname)))

(wild-pathname-p ((:required pathname)
                  (&optional (field-key :default nil)))
                 ((:required generalized-boolean)))


(find-package ((:required name))
              ((:required package)))

(import ((:required symbols)
         (&optional (package :default *package*)))
        ((:required (eql t))))

(intern ((:required string)
         (&optional (package :default *package*)))
        ((:required symbol status)))

(unintern ((:required string)
           (&optional (package :default *package*)))
          ((:required generalized-boolean)))

(list-all-packages ()
                   ((:required packages)))

(make-package ((:required package-name)
               (&key (nicknames :default nil)
                     (use :default :implementation-defined)))
              ((:required package)))

(delete-package ((:required package))
                ((:required generalized-boolean)))

(package-error-package ((:required condition))
                       ((:required package)))

(package-name ((:required package))
              ((:required name)))

(package-nicknames ((:required package))
                   ((:required nicknames)))

(rename-package ((:required package new-name)
                 (&optional (new-nicknames :default nil)))
                ((:required package-object)))

(package-shadowing-symbols ((:required package))
                           ((:required symbols)))

(package-use-list ((:required package))
                  ((:required use-list)))

(package-used-by-list ((:required package))
                      ((:required used-by-list)))

(packagep ((:required object))
          ((:required generalized-boolean)))

(symbol-package ((:required symbol))
                ((:required contents)))

(use-package ((:required packages-to-use)
              (&optional (package :default *package*)))
             ((:required (eql t))))

(unuse-package ((:required packages-to-unuse)
                (&optional (package :default *package*)))
               ((:required (eql t))))

(apropos ((:required string)
          (&optional (package :default nil)))
         ())

(apropos-list ((:required string)
               (&optional (package :default nil)))
              ((:required symbols)))

(copy-symbol ((:required symbol)
              (&optional (copy-properties :default nil)))
             ((:required new-symbol)))

(export ((:required symbols)
         (&optional (package :default *package*)))
        ((:required (eql t))))

(unexport ((:required symbols)
           (&optional (package :default *package*)))
          ((:required (eql t))))

(find-all-symbols ((:required string))
                  ((:required symbols)))

(find-symbol ((:required string)
              (&optional (package :default *package*)))
             ((:required symbol status)))

(gensym ((&optional x))
        ((:required new-symbol)))

(gentemp ((&optional (prefix :default "T")
                     (package :default *package*)))
         ((:required new-symbol)))

(get ((:required symbol indicator)
      (&optional (default :default nil)))
     ((:required value))
     (:store-vars new-value))

(remprop ((:required symbol indicator))
         ((:required generalized-boolean)))

(keywordp ((:required object))
          ((:required generalized-boolean)))

(make-symbol ((:required name))
             ((:required new-symbol)))

(shadow ((:required symbol-names)
         (&optional (package :default *package*)))
        ((:required (eql t))))

(shadowing-import ((:required symbols)
                   (&optional (package :default *package*)))
                  ((:required (eql t))))

(symbol-name ((:required symbol))
             ((:required name)))

(symbol-plist ((:required symbol))
              ((:required plist))
              (:store-vars new-plist))

(symbol-value ((:required symbol))
              ((:required value))
              (:store-vars new-value))

(symbol-function ((:required symbol))
                 ((:required contents))
                 (:store-vars new-contents))

(set ((:required symbol value))
     ((:required value)))

(symbolp ((:required object))
         ((:required generalized-boolean)))

;; FIXME: Order divergence.
((or car cdr caar cadr caaar caadr cadar caddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdar cddr cdaar cdadr cddar cdddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
 ((:required x))
 ((:required object))
 (:store-vars new-object))

(atom ((:required object))
      ((:required generalized-boolean)))

(cons ((:required object-1 object-2))
      ((:required cons)))

(consp ((:required object))
       ((:required generalized-boolean)))

((or rplaca rplacd)
 ((:required cons object))
 ((:required cons)))

((or first second third fourth fifth sixth seventh eighth ninth tenth)
 ((:required list))
 ((:required object))
 (:store-vars new-object))

(adjoin ((:required item list)
         (&key (key :default #'identity)
               (test :default #'eql)
               test-not))
        ((:required new-list)))

;; FIXME: Order divergence.
(append ((&rest lists))
        ((:required result)))

((or revappend nreconc)
 ((:required list tail))
 ((:required result-list)))

(nconc ((&rest lists))
       ((:required concatenated-list)))

((or butlast nbutlast)
 ((:required list)
  (&optional (n :default 1)))
 ((:required result-list)))

(copy-list ((:required list))
           ((:required copy)))

(endp ((:required list))
      ((:required generalized-boolean)))

(last ((:required list)
       (&optional (n :default 1)))
      ((:required tail)))

(ldiff ((:required list object))
       ((:required result-list)))

(list ((&rest objects))
      ((:required list)))

(list* ((&rest+ objects))
       ((:required result)))

(list-length ((:required list))
             ((:required length)))

(listp ((:required object))
       ((:required generalized-boolean)))

(make-list ((:required size)
            (&key (initial-element :default nil)))
           ((:required list)))

;; FIXME: Order divergence.
((or mapc mapl)
 ((:required function)
  (&rest+ lists))
 ((:required list-1)))

((or mapcar maplist)
 ((:required function)
  (&rest+ lists))
 ((:required result-list)))

((or mapcan mapcon)
 ((:required function)
  (&rest+ lists))
 ((:required concatenated-results)))

(member ((:required item list)
         (&key (key :default #'identity)
               (test :default #'eql)))
        ((:required tail)))

((member-if member-if-not)
 ((:required predicate list)
  (&key (key :default #'identity)))
 ((:required tail)))

(nth ((:required n list))
     ((:required object))
     (:store-vars new-object))

(nthcdr ((:required n list))
        ((:required tail)))

(null ((:required object))
      ((:required boolean)))

(not ((:required x))
     ((:required boolean)))

(rest ((:required list))
      ((:required tail))
      (:store-vars new-tail))

(tailp ((:required object list))
       ((:required generalized-boolean)))

(acons ((:required key datum alist))
       ((:required new-alist)))

(copy-alist ((:required alist))
            ((:required new-alist)))

(pairlis ((:required keys data)
          (&optional (alist :default nil)))
         ((:required new-alist)))

(assoc ((:required item alist)
        (&key (key :default #'identity)
              (test :default #'eql)
              test-not))
       ((:required entry)))

((or assoc-if assoc-if-not)
 ((:required predicate alist)
  (&key (key :default #'identity)))
 ((:required entry)))

(rassoc ((:required item alist)
         (&key (key :default #'identity)
               (test :default #'eql)
               test-not))
        ((:required entry)))

((or rassoc-if rassoc-if-not)
 ((:required predicate alist)
  (&key (key :default #'identity)))
 ((:required entry)))

(getf ((:required plist indicator)
       (&optional (default :default nil)))
      ((:required value))
      (:store-vars new-value))

(get-properties ((:required plist indicator-list))
                ((:required indicator value tail)))

(copy-tree ((:required tree))
           ((:required new-tree)))

((or sublis nsublis)
 ((:required alist tree)
  (&key (key :default #'identity)
        (test :default #'eql)
        test-not))
 ((:required new-tree)))

;; FIXME: Order divergence.
((or subst nsubst)
 ((:required new old tree)
  (&key (key :default #'identity)
        (test :default #'eql)
        test-not))
 ((:required new-tree)))

((or subst-if subst-if-not nsubst-if nsubst-if-not)
 ((:required new predicate tree)
  (&key (key :default #'identity)))
 ((:required new-tree)))

(tree-equal ((:required tree-1 tree-2)
             (&key (test :default #'eql)
                   test-not))
            ((:required generalized-boolean)))

(subsetp ((:required list-1 list-2)
          (&key (key :default #'identity)
                (test :default #'eql)
                test-not))
         ((:required generalized-boolean)))

((or union nunion
     intersection nintersection
     set-difference nset-difference
     set-exclusive-or nset-exclusive-or)
 ((:required list-1 list-2)
  (&key (key :default #'identity)
        (test :default #'eql)
        test-not))
 ((:required result-list)))

(count ((:required item sequence)
        (&key (from-end :default nil)
              (start :default 0)
              (end :default nil)
              (key :default #'identity)
              (test :default #'eql)
              test-not))
       ((:required n)))

((or count-if count-if-not)
 ((:required predicate sequence)
  (&key (from-end :default nil)
        (start :default 0)
        (end :default nil)
        (key :default #'identity)))
 ((:required n)))

(find ((:required item sequence)
       (&key (from-end :default nil)
             (start :default 0)
             (end :default nil)
             (key :default #'identity)
             (test :default #'eql)
             test-not))
      ((:required element)))

((or find-if find-if-not)
 ((:required predicate sequence)
  (&key (from-end :default nil)
        (start :default 0)
        (end :default nil)
        (key :default #'identity)))
 ((:required element)))

(position ((:required item sequence)
           (&key (from-end :default nil)
                 (start :default 0)
                 (end :default nil)
                 (key :default #'identity)
                 (test :default #'eql)
                 test-not))
          ((:required position)))

((or position-if position-if-not)
 ((:required predicate sequence)
  (&key (from-end :default nil)
        (start :default 0)
        (end :default nil)
        (key :default #'identity)))
 ((:required position)))

;; FIXME: Order divergence.
((or remove delete)
 ((:required item sequence)
  ;; Order not consistent with previous.
  (&key (from-end :default nil)
        (test :default #'eql)
        test-not
        (start :default 0)
        (end :default nil)
        (count :default nil)
        (key :default #'identity)))
 ((:required result-sequence)))

((or remove-if remove-if-not delete-if delete-if-not)
 ((:required test sequence)
  (&key (from-end :default nil)
        (start :default 0)
        (end :default nil)
        (count :default nil)
        (key :default #'identity)))
 ((:required result-sequence)))

;; FIXME: Order divergence.
((or substitute nsubstitute)
 ((:required newitem olditem sequence)
  ;; Order not consistent with previous.
  (&key (from-end :default nil)
        (test :default #'eql)
        test-not
        (start :default 0)
        (end :default nil)
        (count :default nil)
        (key :default #'identity)))
 ((:required result-sequence)))

((or substitute-if substitute-if-not nsubstitute-if nsubstitute-if-not)
 ((:required newitem predicate sequence)
  (&key (from-end :default nil)
        (start :default 0)
        (end :default nil)
        (count :default nil)
        (key :default #'identity)))
 ((:required result-sequence)))

(concatenate ((:required result-type)
              (&rest sequences))
             ((:required result-sequence)))


(copy-seq ((:required sequence))
          ((:required copied-sequence)))

(elt ((:required sequence index))
     ((:required object))
     (:store-vars new-object))

;; FIXME: Order divergence.
((or every notevery notany)
 ((:required predicate)
  (&rest+ sequences))
 ((:required generalized-boolean)))

(some ((:required predicate)
       (&rest+ sequences))
      ((:required result)))

(fill ((:required sequence item)
       (&key (start :default 0)
             (end :default nil)))
      ((:required sequence)))

(length ((:required sequence))
        ((:required n)))

(make-sequence ((:required result-type size)
                (&key (initial-element :default :implementation-dependent)))
               ((:required sequence)))

(map ((:required result-type function)
      (&rest+ sequences))
     ((:required result)))

(map-into ((:required result-sequence function)
           (&rest sequences))
          ((:required result-sequence)))

(merge ((:required result-type sequence-1 sequence-2 predicate)
        (&key (key :default #'identity)))
       ((:required result-sequence)))

(mismatch ((:required sequence-1 sequence-2)
           (&key (from-end :default nil)
                 (test :default #'eql)
                 test-not
                 (key :default #'identity)
                 (start1 :default 0)
                 (start2 :default 0)
                 (end1 :default nil)
                 (end2 :default nil)))
          ((:required position)))

(reduce ((:required function sequence)
         (&key (key :default #'identity)
               (from-end :default nil)
               (start :default 0)
               (end :default nil)
               initial-value))
        ((:required result)))

((or remove-duplicates delete-duplicates)
 ((:required sequence)
  (&key (from-end :default nil)
        (test :default #'eql)
        test-not
        (start :default 0)
        (end :default nil)
        (key :default #'identity)))
 ((:required result-sequence)))

(replace ((:required sequence-1 sequence-2)
          (&key (start1 :default 0)
                (end1 :default nil)
                (start2 :default 0)
                (end2 :default nil)))
         ((:required sequence-1)))

((or reverse nreverse)
 ((:required sequence))
 ((:required reversed-sequence)))

(search ((:required sequence-1 sequence-2)
         (&key (from-end :default nil)
               (test :default #'eql)
               test-not
               (key :default #'identity)
               (start1 :default 0)
               (start2 :default 0)
               (end1 :default nil)
               (end2 :default nil)))
        ((:required position)))

(subseq ((:required sequence start)
         (&optional (end :default nil)))
        ((:required subsequence))
        (:store-vars new-subsequence))

((or sort stable-sort)
 ((:required sequence predicate)
  (&key (key :default #'identity)))
 ((:required sorted-sequence)))

(alphanumericp ((:required character))
               ((:required generalized-boolean)))

(alpha-char-p ((:required character))
              ((:required generalized-boolean)))

(digit-char ((:required weight)
             (&optional (radix :default 10)))
            ((:required char)))

(digit-char-p ((:required char)
               (&optional (radix :default 10)))
              ((:required weight)))

((or both-case-p lower-case-p upper-case-p)
 ((:required character))
 ((:required generalized-boolean)))

((or char< char<= char/= char= char>= char>
     char-lessp char-not-greaterp char-not-equal char-equal char-not-lessp char-greaterp)
 ((&rest+ characters))
 ((:required generalized-boolean)))

(char-code ((:required character))
           ((:required code)))

(code-char ((:required code))
           ((:required char-p)))

(char-int ((:required character))
          ((:required integer)))

((or char-downcase char-upcase)
 ((:required character))
 ((:required corresponding-character)))

(char-name ((:required character))
           ((:required name)))

(name-char ((:required name))
           ((:required char-p)))

(character ((:required character))
           ((:required denoted-character)))

(characterp ((:required object))
            ((:required generalized-boolean)))

(standard-char-p ((:required character))
                 ((:required generalized-boolean)))

(graphic-char-p ((:required char))
                ((:required generalized-boolean)))

(break ((&optional (format-control :default :implementation-dependent))
        (&rest format-arguments))
       ((:required (eql nil))))

(invoke-debugger ((:required condition))
                 :non-local-exit)

(cell-error-name ((:required condition))
                 ((:required name)))

(make-condition ((:required type)
                 (&rest slot-initializations))
                ((:required condition)))

(signal ((:required datum)
         (&rest arguments))
        ((:required (eql nil))))

(warn ((:required datum)
       (&rest arguments))
      ((:required (eql nil))))

(error ((:required datum)
        (&rest arguments))
       :non-local-exit)

(cerror ((:required continue-format-control datum)
         (&rest arguments))
        ((:required (eql nil))))

;; "Whether method-combination-error returns to its caller
;; or exits via throw is implementation-dependent." ???
(method-combination-error ((:required format-control)
                           (&rest args))
                          :implementation-dependent)

;; See comment for method-combination-error.
(invalid-method-error ((:required method format-control)
                       (&rest args))
                      :implementation-dependent)

(simple-condition-format-control ((:required condition))
                                 ((:required format-control)))

(simple-condition-format-arguments ((:required condition))
                                   ((:required format-arguments)))

(type-error-datum ((:required condition))
                  ((:required datum)))

(type-error-expected-type ((:required condition))
                          ((:required expected-type)))

(unbound-slot-instance ((:required condition))
                       ((:required instance)))

(abort ((&optional (condition :default nil)))
       :non-local-exit)

(continue ((&optional (condition :default nil)))
          ((:required (eql nil))))

(muffle-warning ((&optional (condition :default nil)))
                :non-local-exit)

((or store-value use-value)
 ((:required value)
  (&optional (condition :default nil)))
 ((:required (eql nil))))

(find-restart ((:required identifier)
               (&optional (condition :default nil)))
              ((:required restart)))

(compute-restarts ((&optional (condition :default nil)))
                  ((:required restarts)))

(invoke-restart ((:required restart)
                 (&rest arguments))
                ((&rest results)))

(invoke-restart-interactively ((:required restart))
                              ((&rest results)))

(restart-name ((:required restart))
              ((:required name)))

(make-instance ((:required class)
                (&rest initargs)
                (&key+))
               ((:required instance)))

(allocate-instance ((:required class)
                    (&rest initargs)
                    (&key+))
                   ((:required new-instance)))

;; FIXME: Order divergence.
((or initialize-instance reinitialize-instance)
 ((:required instance)
  (&rest initargs)
  (&key+))
 ((:required instance)))

(shared-initialize ((:required instance slot-names)
                    (&rest initargs)
                    (&key+))
                   ((:required instance)))

(make-instances-obsolete ((:required class))
                         ((:required class)))

(update-instance-for-redefined-class
 ((:required instance added-slots discarded-slots property-list)
  (&rest initargs)
  (&key+))
 ((&rest results)))

(change-class ((:required instance new-class)
               (&key+))
              ((:required instance)))

(update-instance-for-redefined-class ((:required previous current)
                                      (&rest initargs)
                                      (&key+))
                                     :implementation-dependent)

(class-of ((:required object))
          ((:required class)))


(describe ((:required object)
           (&optional (stream :default *standard-output*)))
          ())

(describe-object ((:required object stream))
                 :implementation-dependent)

(slot-value ((:required object slot-name))
            ((:required value))
            ;; CLHS bizarrely labels it as a "function" not "accessor".
            (:store-vars new-value))

(slot-exists-p ((:required object slot-name))
               ((:required generalized-boolean)))

(slot-boundp ((:required instance slot-name))
             ((:required generalized-boolean)))

(slot-makunbound ((:required instance slot-name))
                 ((:required instance)))

(slot-missing ((:required class object slot-name operation)
               (&optional new-value))
              ((&rest results)))

(slot-unbound ((:required class instance slot-name))
              ((&rest results)))

(add-method ((:required generic-function method))
            ((:required generic-function)))

(remove-method ((:required generic-function method))
               ((:required generic-function)))

(compute-applicable-methods ((:required generic-function function-arguments))
                            ((:required methods)))

(find-method ((:required generic-function method-qualifiers specializers)
              (&optional (errorp :default t)))
             ((:required method)))

(function-keywords ((:required method))
                   ((:required keys allow-other-keys-p)))

(method-qualifiers ((:required method))
                   ((:required qualifiers)))

(no-applicable-method ((:required generic-function)
                       (&rest function-arguments))
                      ((&rest results)))

(no-next-method ((:required generic-function method)
                 (&rest args))
                ((&rest results)))

(apply ((:required function)
        (&rest+ args))
       ((&rest results)))

(funcall ((:required function)
          (&rest args))
         ((&rest results)))

(boundp ((:required symbol))
        ((:required generalized-boolean)))

(fboundp ((:required name))
         ((:required generalized-boolean)))

(call-next-method ((&rest args))
                  ((&rest results)))

(next-method-p ()
               ((:required generalized-boolean)))

(coerce ((:required object result-type))
        ((:required result)))

(compile ((:required name)
          (&optional definition))
         ((:required function warnings-p failure-p)))

(compiled-function-p ((:required object))
                     ((:required generalized-boolean)))

(complement ((:required function))
            ((:required complement-function)))

(constantly ((:required value))
            ((:required function)))

(identity ((:required object))
          ((:required object)))

(constantp ((:required form)
            (&optional (environment :default nil)))
           ((:required generalized-boolean)))

(copy-structure ((:required structure))
                ((:required copy)))

(decode-universal-time ((:required universal-time)
                        (&optional time-zone))
                       ((:required second minute hour date month year day daylight-p zone)))

(disassemble ((:required fn))
             ((:required (eql nil))))

(dribble ((&optional pathname))
         :implementation-dependent)

(documentation ((:required x doc-type))
               ((:required documentation))
               (:store-vars new-value))

(ed ((&optional x))
    :implementation-dependent)

((or eq eql equal equalp)
 ((:required x y))
 ((:required generalized-boolean)))

(encode-universal-time ((:required second minute hour date month year)
                        (&optional time-zone))
                       ((:required universal-time)))

(ensure-generic-function ((:required function-name)
                          ;; TODO: Default values
                          (&key argument-precedence-order
                                declare
                                documentation
                                environment
                                generic-function-class
                                lambda-list
                                method-class
                                method-combination))
                         ((:required generic-function)))

(eval ((:required form))
      ((&rest results)))

(find-class ((:required symbol)
             (&optional (errorp :default t)
                        (environment :default nil)))
            ((:required class))
            (:store-vars new-class))

(class-name ((:required class))
            ((:required name)))

(function-lambda-expression ((:required function))
                            ((:required lambda-expression closure-p name)))

(function ((:required object))
          ((:required generalized-boolean)))

(get-decoded-time ()
                  ((:required second minute hour date month year day daylight-p zone)))

(get-internal-real-time ()
                        ((:required internal-time)))

(get-internal-run-time ()
                       ((:required internal-time)))

(get-setf-expansion ((:required place)
                     (&optional (environment :default nil)))
                    ((:required vars vals store-vars writer-form reader-form)))

(get-universal-time ()
                    ((:required universal-time)))

(inspect ((:required object))
         :implementation-dependent)

((or lisp-implementation-type lisp-implementation-version)
 ()
 ((:required description)))

(load ((:required filespec)
       (&key (verbose :default *load-verbose*)
             (print :default *load-print*)
             (if-does-not-exist t)
             (external-format :default :default)))
      ((:required generalized-boolean)))

((or machine-instance machine-type machine-version)
 ()
 ((:required description)))

((or macroexpand macroexpand-1)
 ((:required form)
  (&optional (env :default nil)))
 ((:required expansion expanded-p)))

(make-load-form ((:required object)
                 (&optional (environment :default nil)))
                ((:required creation-form)
                 (&optional initialization-form)))

(make-load-form-saving-slots ((:required object)
                              (&key slot-names
                                    (environment :default nil)))
                             ((:required creation-form initialization-form)))

(not ((:required x))
     ((:required boolean)))

(proclaim ((:required declaration-specifier))
          :implementation-dependent)

(provide ((:required module-name))
         :implementation-dependent)

(require ((:required module-name)
          (&optional (pathname-list :default nil)))
         :implementation-dependent)

(room ((&optional (x :default :default)))
      :implementation-dependent)

((or short-site-name long-site-name)
 ()
 ((:required description)))

(sleep ((:required seconds))
       ((:required (eql nil))))

((or software-type software-version)
 ()
 ((:required description)))

(subtypep ((:required type-1 type-2)
           (&optional (environment :default nil)))
          ((:required subtype-p valid-p)))

(type-of ((:required object))
         ((:required typespec)))

(typep ((:required object type-specifier)
        (&optional (environment :default nil)))
       ((:required generalized-boolean)))

(upgraded-complex-part-type ((:required typespec)
                             (&optional (environment :default nil)))
                            ((:required upgraded-typespec)))

(values ((&rest objects))
        ((&rest objects))
        (:store-vars new-values))

(values-list ((:required list))
             ((&rest elements)))
