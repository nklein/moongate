;;;; moongate.asd

(asdf:defsystem #:moongate
  :description "MOONGATE is another n-dimensional raytracer."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :version "0.1.20231008"
  :depends-on (#:cl-svg)
  :in-order-to ((asdf:test-op (asdf:test-op :moongate/test)))
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "src"
    :components ((:file "package")
                 (:file "size-constants" :depends-on ("package"))
                 (:file "material-constants" :depends-on ("package"))
                 (:file "connection-constants" :depends-on ("package"))
                 (:file "svg-constants" :depends-on ("package"))
                 (:file "tabbed-curves" :depends-on ("package"
                                                     "size-constants"
                                                     "material-constants"
                                                     "connection-constants"))
                 (:file "tabbed-lines" :depends-on ("package"
                                                    "size-constants"
                                                    "material-constants"
                                                    "connection-constants"))
                 (:file "face-shapes" :depends-on ("package"
                                                   "size-constants"
                                                   "material-constants"
                                                   "connection-constants"
                                                   "tabbed-curves"))
                 (:file "edge-shapes" :depends-on ("package"
                                                   "size-constants"
                                                   "material-constants"
                                                   "connection-constants"
                                                   "tabbed-lines"))
                 (:file "draw-moongate" :depends-on ("package"
                                                     "size-constants"
                                                     "material-constants"
                                                     "connection-constants"
                                                     "face-shapes"))))))

(asdf:defsystem #:moongate/test
  :description "Tests for the MOONGATE package."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :version "0.1.20231008"
  :depends-on ((:version #:moongate "0.1.20231008") #:nst)
  :perform (asdf:test-op (o c)
                         (uiop:symbol-call :moongate/test :run-all-tests))
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "test"
    :components ((:file "package")
                 (:file "run" :depends-on ("package"))))))
