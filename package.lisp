(defpackage #:prime-utils
  (:use :cl
        :list-utils)
  (:export :prime-factorization
           :primes-up-to))

(gmath:use-gmath :prime-utils)
