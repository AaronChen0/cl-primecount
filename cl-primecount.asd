(asdf:defsystem #:cl-primecount
  :description "Common Lisp binding for libprimecount"
  :author "Aaron Chen"
  :license "BSD 2-Clause"
  :depends-on ("cffi")
  :components ((:file "primecount")))
