(asdf:defsystem dorg
  :version "0.1.0"
  :description "A flexible documentation generator for common lisp -> .org files"
  :license "FreeBSD"
  :author "Durham Smith <durham@futurefy.io>"
  :maintainer "Durham Smith <durham@futurefy.io>"
  ;;:homepage "https://durhamsmith.github.io/small/"
  :bug-tracker "https://github.com/DurhamSmith/dorg/issues"
  :source-control (:git "https://github.com/DurhamSmith/dorg.git")
  :serial T
  :depends-on (#:defclass-std #:alexandria #:serapeum #:recursive-regex)
  :components ((:file "packages")
               (:file "parser")
               (:file "dorg")))
