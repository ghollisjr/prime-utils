(defpackage #:prime-utils
  (:use
   :cl
   :cl-ana.list-utils
   :cl-generator
   :cl-generator-util
   :intvec)
  (:export
   ;; uses generators
   :primes*
   ;; imperative (faster)
   :prime-factorization
   :primes-sieve))

(cl-ana.gmath:use-gmath :prime-utils)
