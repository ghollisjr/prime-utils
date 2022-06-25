(asdf:defsystem #:prime-utils
  :serial t
  :author "Gary Hollis"
  :license ""
  :description "Basic prime number utilities"
  :depends-on (#:cl-ana.generic-math
               #:cl-ana.list-utils
               #:cl-generator)
  :components ((:file "package")
               (:file "prime-utils")))
