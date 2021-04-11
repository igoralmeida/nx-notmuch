(defpackage #:nx-notmuch
  (:use #:cl #:nyxt)
  (:export :nyxtmuch-show-search-results
           :nyxtmuch--notmuch-search
           :nyxtmuch--thread-css
           :nyxtmuch-show-thread
           :nyxtmuch--notmuch-show-single-thread))
