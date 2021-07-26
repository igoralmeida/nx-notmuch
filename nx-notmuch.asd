;;;; This file is part of nx-notmuch.

;;;; nx-notmuch is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.

;;;; nx-notmuch is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with nx-notmuch.  If not, see <https://www.gnu.org/licenses/>.

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
               (:file "mime")
               (:file "html")
               (:file "notmuch-ffi")
               (:file "notmuch-interface")
               (:file "ui")
               (:file "nx-notmuch")))
