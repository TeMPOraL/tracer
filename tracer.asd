(asdf:defsystem #:tracer
  :author "Jacek TeMPOraL Złydach"
  :description "A tracing and profiling utility"
  :version "0.0.1"
  :license "MIT"

  :homepage "https://github.com/TeMPOraL/tracer"
  :bug-tracker "https://github.com/TeMPOraL/tracer/issues"
  :source-control (:git "https://github.com/TeMPOraL/tracer.git")
  :mailto "tracer@jacek.zlydach.pl"

  :encoding :utf-8

  :depends-on (#:alexandria
               #:bordeaux-threads)

  :pathname "src/"
  :serial t

  :components ((:file "package")
               (:file "trace-event")
               #+sbcl (:file "impl-sbcl")
               (:file "main")))
