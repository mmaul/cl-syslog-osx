(asdf:defsystem #:cl-syslog-osx
  :author "Erik Enge, Mike Maul"
  :version (:read-file-form "VERSION.txt")
  :licence "MIT (See LICENSE)"
  :description "Common Lisp syslog interface."
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-syslog-tests)))
  :depends-on (#:cl-syslog-osx.local #:cl-syslog-osx.udp))

(asdf:defsystem #:cl-syslog-osx.local
  :license "MIT (See LICENSE)"
  :version (:read-file-form "VERSION.txt")
  :description "Local-only syslog logging."
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "variable")
               (:file "cl-syslog")))

(asdf:defsystem #:cl-syslog-osx.udp
  :license "MIT (See LICENSE)"
  :version (:read-file-form "VERSION.txt")
  :description "Local-only syslog logging."
  :depends-on (#:cl-syslog-osx.local #:babel #:cffi #:usocket #:local-time)
  :serial t
  :components ((:file "package-udp")
               (:file "udp-syslog")))


(asdf:defsystem #:cl-syslog-osx-tests
  :description "tests for cl-syslog library"
  :version (:read-file-form "VERSION.txt")
  :author "Mike Maul <mike.maul@gmail.com>"
  :licence "MIT"
  :depends-on (#:cl-syslog-osx #:nst #:cl-ppcre)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :cl-syslog-osx-tests '#:run-all-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "tests")))
