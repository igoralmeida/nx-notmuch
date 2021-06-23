(in-package :nx-notmuch)

(defun notmuch-search (search-string notmuch-args)
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
  (uiop:run-program
   `("notmuch"
     ,@notmuch-args
     "show"
     "--format=json"
     "--body=false"
     ,(str:concat "thread:\"" tid "\""))
   :output #'(lambda (s) (car (yason:parse s)))))
