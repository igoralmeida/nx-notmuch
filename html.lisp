(in-package :nx-notmuch)

;;; tags

(defun format-tags (tags)
  "Group and stylize each tag in TAGS."
  (markup:markup
   (:div :class "tags"
         (loop for tag in tags
               collect
               (format-tag tag)))))

(defun format-tag (tag)
  "Stylize TAG with its automatically assigned foreground/background color."
  (markup:markup
   (:span
    :class "tag"
    :style (let* ((c (get-tag-color tag "#ffffff"))
                  (fc (car c))
                  (bc (cdr c)))
             (str:concat
              "background-color:" bc "; "
              "foreground-color:" fc))
    tag)))

;;;  search

(defun format-search-results (threads)
  "List and style the provided THREADS, with clickable lines."
  (markup:markup
   (:ul
    (loop for thread in threads
          collect
          (markup:markup
           (:a :class "threadli" :href (lisp-url `(nyxtmuch-show ,(getf thread :thread)))
               (:li :class "thread" (markup:raw (format-search-result thread)))))))))

(defun format-search-result (thread)
  "Stylize THREAD as a search result."
  (markup:markup
   (:div
    :style (when (thread-has-unread-p thread)
             "font-weight:bold")
    (:span :class "date" (getf thread :date_relative))
    (:span :class "authors" (getf thread :authors))
    (markup:raw (format-tags (getf thread :tags)))
    (:span :class "subject" (getf thread :subject)))))

(defun thread-has-unread-p (thread)
  "True if THREAD (a plist, probably from `notmuch-search') has an `unread' tag
anywhere."
  (member "unread" (getf thread :tags) :test #'equal))
