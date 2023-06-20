(defpackage #:prime-utils
  (:use
   :cl
   :cl-ana.list-utils
   :cl-generator
   :cl-generator-util)
  (:export
   ;; uses generators
   :primes*
   ;; imperative (faster)
   :prime-factorization
   :primes-sieve
   :prime-count))

(cl-ana.gmath:use-gmath :prime-utils)
