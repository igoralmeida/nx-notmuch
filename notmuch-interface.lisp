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

(in-package :nx-notmuch)

(defun notmuch-search (search-string notmuch-args)
  "Run notmuch search with SEARCH-STRING.

Hard-coded limit of 300 results."
  (uiop:run-program
   `("notmuch"
     ,@notmuch-args
     "search"
     "--format=sexp"
     "--limit=300" ;;TODO remove this and use some other facility to prevent huge searches
     ,search-string)
   :force-shell t
   :output :form))

(defun notmuch-show-single-thread (tid notmuch-args)
  "Run `notmuch show thread:TID' to retrieve that thread."
  (uiop:run-program
   `("notmuch"
     ,@notmuch-args
     "show"
     "--format=json"
     "--body=false"
     ,(str:concat "thread:\"" tid "\""))
   :output #'(lambda (s) (first (yason:parse s)))))
