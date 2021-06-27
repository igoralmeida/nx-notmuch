;;;; nx-notmuch.asd

(asdf:defsystem #:nx-notmuch
  :description "A notmuch interface for nyxt"
  :author "Igor Almeida"
  :serial t
  :depends-on (#:nyxt
               :parenscript
               "md5" "simple-rgb" "cl-mime" "yason")
  :components ((:file "package")
               (:file "utils")
               (:file "html")
               (:file "notmuch-interface")
               (:file "nx-notmuch")))
