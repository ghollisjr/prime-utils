(defpackage #:prime-utils
  (:use :cl
        :cl-ana.list-utils)
  (:export
   :primes
   :prime-factorization
   :primes-up-to
   :primes-sieve))

(cl-ana.gmath:use-gmath :prime-utils)
