(in-package :nx-notmuch)

(serapeum:export-always '*nyxtmuch*)
(defvar *nyxtmuch* nil
  "Global nyxtmuch config")

(nyxt::define-class nyxtmuch-search-source (prompter:source)
  ((prompter:name "Nyxtmuch search history")
   (prompter:constructor (lambda (s)
                           (declare (ignore s))
                           (containers:container->list (slot-value *nyxtmuch* 'search-history))))
   ;;TODO still doesn't select the history item if current input matches
   (prompter:filter-preprocessor (lambda (suggestions source input)
                                   (declare (ignore suggestions))
                                   (append
                                    (list (funcall (prompter:suggestion-maker source) input
                                                   source input))
                                    (containers:container->list (slot-value *nyxtmuch* 'search-history)))))
   (prompter:filter (lambda (suggestion source input)
                      (prompter:submatches suggestion source input)))))

(nyxt::define-class nyxtmuch-config ()
  ((notmuch-args nil
                 :type list
                 :documentation "List of strings, args that are passed to
  notmuch calls")
   (search-history :type containers:ring-buffer-reverse
                   :initform (containers:make-ring-buffer 100 :last-in-first-out)
                   :documentation "Search history."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(setf *nyxtmuch*
      (make-instance
       'nyxtmuch-config
       :notmuch-args (list (str:concat
                            "--config="
                            (uiop:native-namestring "~/.notmuch-config")))))

(nyxt:define-mode nyxtmuch-show-mode ()
  "mode for nyxtmuch threads"
  ((keymap-scheme
    (nyxt::define-scheme "nm-show"
      scheme:vi-normal
      (list
       "c" 'nyxtmuch-collapse-message)))))

(nyxt:define-mode nyxtmuch-search-mode ()
  "mode for nyxtmuch searches"
  ((keymap-scheme
    (nyxt::define-scheme "nm-search"
      scheme:vi-normal
      (list
       "r" 'nyxt::nyxtmuch-render-search)))))

(defun nyxtmuch--get-tag-color (tag-name canvas-color)
  "Calculates a tag color for a given TAG-NAME

This is a port of astroid's Utils::get_tag_color_rgba(). "
  (let* ((upper "#e5e5e5")
         (lower "#333333")
         (alpha 0.5)
         (upperl (rgb:parse upper))
         (lowerl (rgb:parse lower))
         (cv (rgb:parse canvas-color))
         (tag-digest (md5:md5sum-string tag-name))
         (bg-lambda (lambda (dig u l)
                      ;;TODO don't know how to do 8-bit arithmetic without (mod...) here
                      (mod (+ l (mod (* dig (- u l)) 256)) 256)))
         (bg (rgb:rgb (funcall bg-lambda (aref tag-digest 0) (aref upperl 0) (aref lowerl 0))
                      (funcall bg-lambda (aref tag-digest 1) (aref upperl 1) (aref lowerl 1))
                      (funcall bg-lambda (aref tag-digest 2) (aref upperl 2) (aref lowerl 2))))
         (bc (rgb:lighten-rgb bg))
         (deemph (lambda (b a c)
                   (+ (* b a)
                      (* c (- 1 a)))))
         (lum (/ (+
                  (* 0.2126 (funcall deemph (aref bg 0) alpha (aref cv 0)))
                  (* 0.7152 (funcall deemph (aref bg 1) alpha (aref cv 1)))
                  (* 0.0722 (funcall deemph (aref bg 2) alpha (aref cv 0))))
                 255.0))
         (fc (if (> lum 0.5)
                 "#000000"
                 "#f2f2f2")))
    (cons fc (rgb:xmlify-rgb bc))))

(defun nyxtmuch--format-message-body-part (mime-part)
  "Recursively format"
  (flet ((mime->string (m)
           (str:concat
            (cl-mime:content-type m) "/" (cl-mime:content-subtype m)))
         (part-fetcher (m)
           (ecase (cl-mime:content-transfer-encoding m)
             ((:8bit :7bit) (cl-mime:content m))
             (:base64
              (flexi-streams:octets-to-string (cl-mime:decode-content m)))
             (:quoted-printable
              (flexi-streams:octets-to-string (cl-mime:decode-content m))))))
    (if (not (typep mime-part 'cl-mime:mime))
        (markup:markup (:blockquote :style "background-color:red" "MIME TYPE ERROR"))

        (let ((part-type (mime->string mime-part)))
          (cond
            ((string-equal part-type "text/plain")
             (markup:markup
              (:div :style "white-space:pre" (part-fetcher mime-part))))

            ((string-equal part-type "text/html")
             (markup:raw (part-fetcher mime-part)))

            ((or (string-equal part-type "multipart/signed")
                 (string-equal part-type "multipart/mixed"))
             (markup:markup
              (:div :class "multipart"
                    (loop for part in (cl-mime:content mime-part)
                          collect
                          (nyxtmuch--format-message-body-part part)))))

            ((string-equal part-type "multipart/alternative")
             (flet ((part-preferrer (alts)
                      (or (find-if (serapeum:equals "text/html")
                                   alts
                                   :key #'mime->string)
                          (find-if (serapeum:equals "text/plain")
                                   (cl-mime:content alts)
                                   :key #'mime->string))))
               (markup:markup
                (:div :class "alt"
                      (nyxtmuch--format-message-body-part
                       (part-preferrer (cl-mime:content mime-part)))))))

            (t (markup:markup
                (:blockquote :style "background-color:yellow"
                             (str:concat "UNKNOWN MIME TYPE " part-type)))))))))

(defun nyxtmuch--format-message-body (mail-file)
  (with-open-file (msg (uiop:ensure-pathname mail-file) :direction :input)
    (let ((mime (cl-mime:parse-mime msg)))
      (markup:markup
       (:div (markup:raw
              (nyxtmuch--format-message-body-part mime)))))))

(defun nyxtmuch--notmuch-search (search-string notmuch-args)
  (uiop:run-program
   `("notmuch"
     ,@notmuch-args
     "search"
     "--format=sexp"
     "--limit=300" ;;TODO remove this and use some other facility to prevent huge searches
     ,search-string)
   :force-shell t
   :output :form))

(defun nyxtmuch--notmuch-show-single-thread (tid notmuch-args)
  (uiop:run-program
   `("notmuch"
     ,@notmuch-args
     "show"
     "--format=json"
     "--body=false"
     ,(str:concat "thread:\"" tid "\""))
   :output #'(lambda (s) (car (yason:parse s)))))

(defun nyxtmuch--format-tag (tag)
  (markup:markup
   (:span
    :class "tag"
    :style (let* ((c (nyxtmuch--get-tag-color tag "#ffffff"))
                 (fc (car c))
                 (bc (cdr c)))
             (str:concat
              "background-color:" bc "; "
              "foreground-color:" fc))
    tag)))

(defun nyxtmuch--format-tags (tags)
  (markup:markup
   (:div :class "tags"
         (loop for tag in tags
                 collect
                 (nyxtmuch--format-tag tag)))))

(defun nyxtmuch--thread-has-unread-p (thread)
  (member "unread" (getf thread :tags) :test #'equal))

(defun nyxtmuch--format-thread (thread)
  (markup:markup
   (:div
    :style (when (nyxtmuch--thread-has-unread-p thread)
             "font-weight:bold")
    (:span :class "date" (getf thread :date_relative))
    (:span :class "authors" (getf thread :authors))
    (markup:raw (nyxtmuch--format-tags (getf thread :tags)))
    (:span :class "subject" (getf thread :subject)))))

(defun nyxtmuch--format-message (message-ht)
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
    (setq formatted-body (nyxtmuch--format-message-body (car (gethash "filename" message-ht))))
    (markup:markup
     (:div
      (:ul :class "headers"
           (:li (:span :class "header-label" "From:") (:b (:span from)))
           (:li (:span :class "header-label" "To:") (:span to))
           (:li (:span :class "header-label" "Date:") (:span date))
           (:li (:span :class "header-label" "Subject:") (:span subject))
           (:li (:span :class "header-label" "Tags:") (markup:raw (nyxtmuch--format-tags tags))))
      (:hr)
      (:div :class "messagebody" (markup:raw formatted-body))))))

(defun nyxtmuch--thread-id (thread)
  (getf thread :thread))

(defun nyxtmuch--make-thread-href (thread)
  (lisp-url `(nyxt::nyxtmuch-show ,(nyxtmuch--thread-id thread))))

(defun nyxtmuch-show-search-results (threads)
  (markup:markup
   (:ul
    (loop for thread in threads
          collect
          (markup:markup
           (:a :class "threadli" :href (nyxtmuch--make-thread-href thread)
               (:li :class "thread" (markup:raw (nyxtmuch--format-thread thread)))))))))

(defun nyxtmuch-show-thread (messages)
  (markup:markup
   (:ul
    (loop for (parent children) in messages
          collect
          (str:concat
           (markup:markup
            (:li :class "message" (markup:raw (nyxtmuch--format-message parent))))
           (if children
               (markup:markup (:div (markup:raw (nyxtmuch-show-thread children))))
               "")
           )))))

(defvar nyxtmuch--thread-css
  (cl-css:css
   '(("body"
      :background-color "#eee")
     ("li.message"
      :background-color "white"
      :width "90%")
     ("ul.headers"
      :list-style-type "none"
      :margin 0
      :padding "10px")
     ("span.header-label"
      :color "gray"
      :font-size "75%"
      :margin "2px")
     ("blockquote"
      :border-left "3px solid blue"
      :padding "0.5em 3px")
     ("div.tags"
      :margin "2px"
      :display "inline-block")
     ("span.tag"
      :margin "2px"
      :padding "2px 0.1em")
     ))
  "The CSS definitions for the thread buffer")

(nyxt::define-class nyxtmuch-search-buffer (user-internal-buffer)
  ((search-string "" :type string :documentation "Notmuch search string.")
   (style #.(cl-css:css
             '(("li.thread:hover"
                :background-color "#eee")
               ("li.message:hover"
                :background-color "#eee")
               ("a.threadli"
                :color "unset")
               ("a.messageli"
                :color "unset")
               ("span.date"
                :margin "2px"
                :display "inline-block"
                :width "10em")
               ("span.authors"
                :margin "2px"
                :display "inline-block"
                :width "40em")
               ("span.subject"
                :margin "2px"
                :display "inline-block")
               ("div.tags"
                :margin "2px"
                :display "inline-block")
               ("span.tag"
                :margin "2px"
                :padding "2px 0.1em")))))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defmethod default-modes append ((buffer nyxtmuch-search-buffer))
  '(nyxtmuch-search-mode))

(nyxt:define-command nyxt::nyxtmuch-render-search (&optional buffer)
  "Build the nyxtmuch search associated with this buffer"
  (let* ((buffer (or buffer (current-buffer)))
         (style (style buffer))
         (search-string (search-string buffer)))
    (with-current-buffer buffer
      (nyxt::html-set
       (str:concat
        (markup:markup
         (:style (slot-value (nyxt::make-dummy-buffer) 'style))
         (:style style)
         (:h1 "Nyxtmuch")
         (:p search-string)
         (:hr))
        (nyxtmuch-show-search-results
         (nyxtmuch--notmuch-search
          search-string
          (slot-value *nyxtmuch* 'notmuch-args))))))))

(nyxt:define-command nyxt::nyxtmuch-search ()
  "Open nyxtmuch with some search"
  (let* ((search-hist (slot-value *nyxtmuch* 'search-history))
         (search-result
           (nyxt:prompt
            :prompt "Notmuch search:"
            :history search-hist
            :sources 'nyxtmuch-search-source))
         (search-term (if (listp search-result)
                          (first search-result)
                          search-result))
         (buffer-name (str:concat "*Nyxtmuch search*<" search-term ">"))
         (thebuf (nyxt::buffer-make *browser*
                 :title buffer-name
                 :buffer-class 'nyxtmuch-search-buffer)))
    (setf (slot-value thebuf 'search-string) search-term)
    (nyxt::nyxtmuch-render-search thebuf)
    (set-current-buffer thebuf)))

(nyxt:define-command nyxt::nyxtmuch-show (tid)
  "Show a thread in nyxtmuch"
  (let ((buffer-name (str:concat "*Nyxtmuch show*<thread:" tid ">")))
    (with-current-html-buffer (buffer buffer-name 'nyxtmuch-show-mode)
      (str:concat
       (markup:markup
        (:style (style buffer))
        (:style nyxtmuch--thread-css)
        (:h1 "Nyxtmuch")
        (:p (str:concat "thread:" tid))
        (:hr))
       (nyxtmuch-show-thread
        (nyxtmuch--notmuch-show-single-thread
         tid
         (slot-value *nyxtmuch* 'notmuch-args)))))))

;; (define-parenscript %collapse-message (message-element)
;;   (defun qs (context selector)
;;     "Alias of document.querySelector"
;;     (ps:chain context (query-selector selector)))

;;   (let ((bodydiv (qs (ps:lisp message-element) "div.messagebody")))
;;     (setf (ps:chain bodydiv style display) "none")))

;; (define-command nyxtmuch-collapse-message ()
;;   "Collapses a message shown in a thread buffer"
;;   (nyxt/web-mode:query-hints "Collapse which message?" '%collapse-message))
