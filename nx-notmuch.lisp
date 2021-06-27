(in-package :nx-notmuch)

(serapeum:export-always '*nyxtmuch*)
(defvar *nyxtmuch* nil
  "Global nyxtmuch config")

(define-class search-source (prompter:source)
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

(define-class config ()
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
       'config
       :notmuch-args (list (str:concat
                            "--config="
                            (uiop:native-namestring "~/.notmuch-config")))))

(define-mode nyxtmuch-show-mode ()
  "mode for nyxtmuch threads"
  ((keymap-scheme
    (define-scheme "nm-show"
      scheme:vi-normal
      (list
       "c" 'nyxtmuch-collapse-message
       "r" 'nyxtmuch-render-thread)))))

(define-mode nyxtmuch-search-mode ()
  "mode for nyxtmuch searches"
  ((keymap-scheme
    (define-scheme "nm-search"
      scheme:vi-normal
      (list
       "r" 'nyxtmuch-render-search)))))


(defun nyxtmuch--format-message-body-part (mime-part)
  "Recursively format"
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
                        (nyxtmuch--format-message-body-part part)))))

          ((string-equal part-type "multipart/alternative")
           (markup:markup
            (:div :class "alt"
                  (nyxtmuch--format-message-body-part
                   (select-mime-alternative (cl-mime:content mime-part))))))

          (t (markup:markup
              (:blockquote :style "background-color:yellow"
                           (str:concat "UNKNOWN MIME TYPE " part-type))))))))

(defun nyxtmuch--format-message-body (mail-file)
  ;TODO Looks nice, but I don't like that we keep the file open while building
  ;the markup
  (with-open-file (msg (uiop:ensure-pathname mail-file) :direction :input)
    (let ((mime (cl-mime:parse-mime msg)))
      (markup:markup
       (:div (markup:raw
              (nyxtmuch--format-message-body-part mime)))))))

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
           (:li (:span :class "header-label" "Tags:") (markup:raw (format-tags tags))))
      (:hr)
      (:div :class "messagebody" (markup:raw formatted-body))))))

(defun format-thread (messages)
  "Recursively format the thread represented by MESSAGES, as nested lists."
  (markup:markup
   (:ul
    (loop for (parent children) in messages
          collect
          (str:concat
           (markup:markup
            (:li :class "message" (markup:raw (nyxtmuch--format-message parent))))
           (when children
             (markup:markup (:div (markup:raw (format-thread children))))))))))

(define-class show-buffer (user-internal-buffer)
  ((thread-id "" :type string :documentation "Notmuch thread id.")
   (style #.(cl-css:css
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
                :padding "2px 0.1em")))))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defmethod default-modes append ((buffer show-buffer))
  '(nyxtmuch-show-mode))

(define-class search-buffer (user-internal-buffer)
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

(defmethod default-modes append ((buffer search-buffer))
  '(nyxtmuch-search-mode))

(define-command-global nyxtmuch-render-search (&optional buffer)
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
        (format-search-results
         (notmuch-search
          search-string
          (slot-value *nyxtmuch* 'notmuch-args))))))))

(define-command-global nyxtmuch-search ()
  "Open nyxtmuch with some search"
  (let* ((search-hist (slot-value *nyxtmuch* 'search-history))
         (search-result
           (nyxt:prompt
            :prompt "Notmuch search:"
            :history search-hist
            :sources 'search-source))
         (search-term (if (listp search-result)
                          (first search-result)
                          search-result))
         buffer-name thebuf)
    (if (str:emptyp search-term)
        (echo "Notmuch search cannot be empty.")
        (progn
          (setf buffer-name (str:concat "*Nyxtmuch search*<" search-term ">"))
          (setf thebuf (nyxt::buffer-make *browser*
                                          :title buffer-name
                                          :buffer-class 'search-buffer))
          (setf (slot-value thebuf 'search-string) search-term)
          (nyxtmuch-render-search thebuf)
          (set-current-buffer thebuf)))))

(define-command-global nyxtmuch-render-thread (&optional buffer)
  "Build the nyxtmuch thread view associated with this buffer"
  (let* ((buffer (or buffer (current-buffer)))
         (style (style buffer))
         (thread-id (thread-id buffer)))
    (with-current-buffer buffer
      (nyxt::html-set
       (str:concat
        (markup:markup
         (:style (slot-value (nyxt::make-dummy-buffer) 'style))
         (:style style)
         (:h1 "Nyxtmuch")
         (:p (str:concat "thread:" thread-id))
         (:hr))
        (format-thread
         (notmuch-show-single-thread
          thread-id
          (slot-value *nyxtmuch* 'notmuch-args))))))))

(define-command-global nyxtmuch-show (tid)
  "Show a thread in nyxtmuch"
  (let* ((buffer-name (str:concat "*Nyxtmuch show*<thread:" tid ">"))
         (thebuf (nyxt::buffer-make *browser*
                                    :title buffer-name
                                    :buffer-class 'show-buffer)))
    (setf (slot-value thebuf 'thread-id) tid)
    (nyxtmuch-render-thread thebuf)
    (set-current-buffer thebuf)))

;; (define-parenscript %collapse-message (message-element)
;;   (defun qs (context selector)
;;     "Alias of document.querySelector"
;;     (ps:chain context (query-selector selector)))

;;   (let ((bodydiv (qs (ps:lisp message-element) "div.messagebody")))
;;     (setf (ps:chain bodydiv style display) "none")))

;; (define-command nyxtmuch-collapse-message ()
;;   "Collapses a message shown in a thread buffer"
;;   (nyxt/web-mode:query-hints "Collapse which message?" '%collapse-message))
