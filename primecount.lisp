(defpackage :cl-primecount
  (:nicknames #:cl-pc)
  (:use #:cl #:cffi)
  (:export #:primepi
           #:primepi-str
           #:phi
           #:get-max-x
           #:nth-prime
           #:get-num-threads
           #:set-num-threads
           #:version))

(in-package :cl-primecount)

(define-foreign-library libprimecount
  (:darwin (:or "libprimecount.dylib" "libprimecount.6.dylib" "libprimecount.5.dylib"))
  (:unix (:or "libprimecount.so" "libprimecount.so.6" "libprimecount.so.5"))
  (t (:default "libprimecount")))

(defun load-lib ()
  (handler-case
      (use-foreign-library libprimecount)
    (error (e)
      (declare (ignore e))
      (warn "libprimecount not loaded."))))

(load-lib)

;;; Alias in case size_t changes.
(defctype size :unsigned-int)

;; Count the number of primes <= x using Xavier Gourdon's
;; algorithm. Uses all CPU cores by default.
;;
;; Returns -1 if an error occurs.
;; Run time: O(x^(2/3) / (log x)^2)
;; Memory usage: O(x^(1/3) * (log x)^3)
(defcfun ("primecount_pi" primepi) :int64
  (x :int64))

;; 128-bit prime counting function, inputs could be like 1000000/10^6/1e6.
(defun primepi-str (str)
  (declare (type simple-string str))
  (let* ((res (foreign-alloc :char :count 32))
         (ret (foreign-funcall "primecount_pi_str" :string str
                               :pointer res size 32 :int)))
    (when (= ret -1)
      (return-from primepi-str -1))
    (prog1 (parse-integer (foreign-string-to-lisp res))
      (foreign-free res))))

;; Partial sieve function (a.k.a. Legendre-sum).
;; phi(x, a) counts the numbers <= x that are not divisible
;; by any of the first a primes.
;; Returns -1 if an error occurs.
(defcfun ("primecount_phi" phi) :int64
  (x :int64)
  (a :int64))

;; Find the nth prime using a combination of the prime counting
;; function and the sieve of Eratosthenes.
;; @pre n <= 216289611853439384
;; Returns -1 if an error occurs.
;; 
;; Run time: O(x^(2/3) / (log x)^2)
;; Memory usage: O(x^(1/2))
(defcfun ("primecount_nth_prime" nth-prime) :int64
  (n :int64))

;; Largest number supported by primecount_pi_str(x).
;; @return 64-bit CPUs: 10^31,
;;         32-bit CPUs: 2^63-1
(defcfun ("primecount_get_max_x" get-max-x) :string)

;; Get the currently set number of threads
(defcfun ("primecount_get_num_threads" get-num-threads) :int)

;; Set the number of threads
(defcfun ("primecount_set_num_threads" set-num-threads) :void
  (num-threads :int))

;; Get the libprimecount version number, in the form “i.j”
(defcfun ("primecount_version" version) :string)
