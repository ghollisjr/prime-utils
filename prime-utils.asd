(asdf:defsystem #:prime-utils
  :serial t
  :author "Gary Hollis"
  :license ""
  :description "Basic prime number utilities"
  :depends-on (#:generic-math
               #:list-utils)
  :components ((:file "package")
               (:file "prime-utils")))
