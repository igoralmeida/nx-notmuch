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

;;;  show

(defun format-thread (messages)
  "Recursively format the thread represented by MESSAGES, as nested lists."
  (markup:markup
   (:ul
    (loop for (parent children) in messages
          collect
          (str:concat
           (markup:markup
            (:li :class "message" (markup:raw (format-message parent))))
           (when children
             (markup:markup (:div (markup:raw (format-thread children))))))))))

(defun format-message (message-ht)
  "Stylize MESSAGE-HT, including its body."
  (let (headers-ht formatted-body from to date subject tags)
    (handler-case
        (progn
          (setq headers-ht (gethash "headers" message-ht))
          (setq tags (gethash "tags" message-ht))
          (setq subject (gethash "Subject" headers-ht))
          (setq from (gethash "From" headers-ht))
          (setq to (gethash "To" headers-ht))
          (setq date (gethash "Date" headers-ht)))
      (t (c)
        (format t "Got an exception: ~a~%" c)
        (unless from (setq from "ERROR"))
        (unless to (setq to "ERROR"))
        (unless date (setq date (gethash "date_relative" message-ht)))
        (unless (str:non-empty-string-p subject) (setq subject "ERROR"))))
    (setq formatted-body (format-message-body (car (gethash "filename" message-ht))))
    (markup:markup
     (:div
      (:ul :class "headers"
           (:li (:span :class "header-label" "From:") (:b (:span from)))
           (:li (:span :class "header-label" "To:") (:span to))
           (:li (:span :class "header-label" "Date:") (:span date))
           (:li (:span :class "header-label" "Subject:") (:span subject))
           (:li (:span :class "header-label" "Tags:") (markup:raw (format-tags tags))))
      (:hr)
      (:div :class "messagebody" (markup:raw formatted-body))))))

(defun format-message-body (mail-file)
  "Stylize the contents of MAIL-FILE."
  (with-open-file (msg (uiop:ensure-pathname mail-file) :direction :input)
      ;TODO Looks nice, but I don't like that we keep the file open while
      ;building the markup. Maybe use the mime object as input instead?
    (let ((mime (cl-mime:parse-mime msg)))
      (markup:markup
       (:div (markup:raw
              (format-message-body-part mime)))))))

(defun format-message-body-part (mime-part)
  "Recursively format MIME-PART, depending on its type."
  (if (not (typep mime-part 'cl-mime:mime))
      (markup:markup (:blockquote :style "background-color:red" "MIME TYPE ERROR"))

      (let ((part-type (full-mime-type mime-part)))
        (cond
          ((string-equal part-type "text/plain")
           (markup:markup
            (:div :style "white-space:pre" (get-mime-content mime-part))))

          ((string-equal part-type "text/html")
           (markup:raw (get-mime-content mime-part)))

          ((or (string-equal part-type "multipart/signed")
               (string-equal part-type "multipart/mixed"))
           (markup:markup
            (:div :class "multipart"
                  (loop for part in (cl-mime:content mime-part)
                        collect
                        (format-message-body-part part)))))

          ((string-equal part-type "multipart/alternative")
           (markup:markup
            (:div :class "alt"
                  (format-message-body-part
                   (select-mime-alternative (cl-mime:content mime-part))))))

          (t (markup:markup
              (:blockquote :style "background-color:yellow"
                           (str:concat "UNKNOWN MIME TYPE " part-type))))))))
