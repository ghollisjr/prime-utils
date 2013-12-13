(defpackage #:prime-utils
  (:use :cl
        :list-utils)
  (:export :prime-factor
           :primes-up-to))

(gmath:use-gmath :prime-utils)
