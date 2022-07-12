(defpackage #:prime-utils
  (:use :cl
   :cl-ana.list-utils)
  (:shadowing-import-from :cl-generator
                          :defun*
                          :iter-next
                          :yield
                          :yield*)
  (:export
   :primes
   :prime-factorization
   :primes-up-to
   :primes-sieve))

(cl-ana.gmath:use-gmath :prime-utils)
