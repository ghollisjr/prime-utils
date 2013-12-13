(in-package :prime-utils)

;; this function could use optimizing, it looks like a slight
;; modification of the sieve of eratosthenes.
(defun primes-up-to (n)
  "Returns a list of the prime numbers less than or equal to n."
  (if (< n 2)
      nil
      (do* ((candidates (range 2 n)
                        (remove-if (lambda (x)
                                     (zerop (mod x p)))
                                   candidates))
            (result () (cons p result))
            (p 2 (first candidates)))
           ((null p) (nreverse result)))))

(defun prime-factor (n)
  (let ((candidates (primes-up-to (sqrt n)))
        (x n)
        (p 2)
        (result ()))
    (do ()
        ((null candidates))
      (if (zerop (mod x p))
          (progn
            (setf x (floor x p))
            (push p result))
          (progn
            (setf candidates (rest candidates))
            (setf p (first candidates)))))
    (when (not (= 1 x))
      (push x result))
    (if (null result)
        (list (cons n 1))
        (compress result :singleton-pairs t))))
