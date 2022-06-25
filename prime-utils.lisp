(in-package :prime-utils)

;; Generator for prime numbers.  Need to fix defun* doc string
;; handling.
(cl-generator:defun* primes ()
  "Generator for prime numbers"
  (let* ((primes NIL)
         (last-cons NIL))
    (labels ((add-prime (p)
               (if (null last-cons)
                   (setf primes (list p)
                         last-cons primes)
                   (setf (cdr last-cons) (list p)
                         last-cons (cdr last-cons)))))
      (loop
        for i from 2
        do
           (block prime-test
             (do* ((ps primes (rest ps))
                   (p (first ps) (first ps)))
                  ((or (null ps)
                       (> p (sqrt i)))
                   (add-prime i)
                   (cl-generator:yield i))
               (when (zerop (mod i p))
                 (return-from prime-test NIL))))))))

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

;; Direct sieve algorithm
(defun primes-sieve (n)
  "Uses sieve approach to generate primes.  Memory intensive, but
relatively fast."
  (let* ((continue t)
         (prime-index 2)
         (primes nil)
         (candidates (make-array n :element-type 'boolean
                                 :initial-element t)))
    ;; initialize candidates
    (setf (aref candidates 0) nil) ; 1 isn't prime
    ;; initialize primes
    (push 2 primes)
    ;; loop
    (loop
       while continue
       do
         (progn
           ;; mark elements
           (loop
              for i = (* 2 prime-index) then (+ i prime-index)
              while (<= i n)
              do
                (when (aref candidates (1- i))
                  (setf (aref candidates (1- i))
                        nil)))
           ;; get next prime
           (let* ((next
                   (position t candidates :start prime-index)))
             (if next
                 (progn
                   (incf next)
                   (push next primes)
                   (setf prime-index next))
                 (setf continue nil)))))
    (reverse primes)))

;; Could be improved by not using primes-up-to, but instead creating a
;; closure that would generate each prime sequentially.  Would
;; potentially need a different algorithm to generate the primes.
;;
;; UPDATE: I have a primes generator, which is what the above would
;; need.  Should make another version and compare performance.
(defun prime-factorization (n)
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
    (sort (if (null result)
              (list (cons n 1))
              (compress result :singleton-pairs t))
          #'< :key #'first)))
