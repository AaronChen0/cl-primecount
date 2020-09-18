# cl-primecount

Common Lisp bindings for the highly optimized and multi-threaded [primecount](https://github.com/kimwalisch/primecount) C++ library.

## API as examples

```common-lisp
(defpackage :cl-pc-test (:use :cl :cl-primecount))
(in-package :cl-pc-test)

;; Count the number of primes <= 10^14. Uses all CPU cores by default.
(primepi (expt 10 14))

;; 128-bit version of primepi, inputs could be like 100000000000000/10^14/1e14.
(primepi-str "100000000000000")
(primepi-str "1e14")
(primepi-str "10^14")

;; Partial sieve function (a.k.a. Legendre-sum).
;; phi(x, a) counts the numbers <= x that are not divisible
;; by any of the first a primes.
(phi 100 5)

;; Find the nth prime.
(nth-prime (expt 10 10))

;; Get the largest number supported by primepi-str.
(get-max-x)

;; Get the currently set number of threads.
(get-num-threads)

;; Set the number of threads.
(set-num-threads 2)

;; Get the libprimecount version number, in the form “i.j”.
(version)
```
For convenience, package cl-primecount has nickname cl-pc.

## Installation

Follow the build instructions from [primecount](https://github.com/kimwalisch/primecount/blob/master/doc/BUILD.md) to get libprimecount installed. And then
``` shell
cd ~/quicklisp/local-projects/
git clone https://github.com/AaronChen0/cl-primecount.git
```

In a lisp repl:
``` common-lisp
(ql:quickload "cl-primecount")
```
