Basic prime number utilities.

API summary:

* primes*: Generator for prime numbers.
* primes-sieve: Primes sieve algorithm.  High memory use, fast speed.
* prime-factorization: Find prime factors of argument using
  primes-sieve.  Can supply list of prime numbers to avoid re-sieving.

Internal example functions:
* primes-up-to*: Example of using primes* generator, but slower than
  primes-sieve.

* primes-up-to: Imperative version of primes-up-to*.  Useful for
  benchmarking generator overhead.
