(defpackage #:prime-utils
  (:use :cl
   :cl-ana.list-utils)
  (:use :cl-generator
        :cl-generator-util)
  (:export
   ;; uses generators
   :primes*
   ;; imperative (faster)
   :prime-factorization
   :primes-sieve))

(cl-ana.gmath:use-gmath :prime-utils)
