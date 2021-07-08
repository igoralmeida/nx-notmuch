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
       "C-j" 'nyxtmuch-focus-next-message
       "C-k" 'nyxtmuch-focus-prev-message
       "r" 'nyxtmuch-render-thread)))))

(define-mode nyxtmuch-search-mode ()
  "mode for nyxtmuch searches"
  ((keymap-scheme
    (define-scheme "nm-search"
      scheme:vi-normal
      (list
       "r" 'nyxtmuch-render-search)))))

(define-class show-buffer (user-internal-buffer)
  ((thread-id "" :type string :documentation "Notmuch thread id.")
   ;TODO could this go to html.lisp, somehow?
   (style #.(cl-css:css
             '(("body"
                :background-color "#eee")
               ("li.message"
                :background-color "white"
                :width "90%")
               ("li.message > div"
                :padding-left "0.5em"
                :border-left "2px solid white")
               ("li.message.selected > div"
                :border-left "2px solid blue")
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
   ;TODO could this go to html.lisp, somehow?
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
               ("div.result.unread"
                :font-weight "bold")
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

(define-command nyxtmuch-render-search (&optional buffer)
  "Build the nyxtmuch search associated with this buffer."
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
  "Open nyxtmuch with some search."
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

(define-command nyxtmuch-render-thread (&optional buffer)
  "Build the nyxtmuch thread view associated with this buffer."
  (let* ((buffer (or buffer (current-buffer)))
         (style (style buffer))
         (thread-id (thread-id buffer)))
    (with-current-buffer buffer
      (nyxt::html-set
       (str:concat
        (let ((markup:*auto-escape* nil))
          (markup:markup
           (:style (slot-value (nyxt::make-dummy-buffer) 'style))
           (:style style)
           (:h1 "Nyxtmuch")
           (:p (str:concat "thread:" thread-id))
           (:hr)))
        (format-thread
         (notmuch-show-single-thread
          thread-id
          (slot-value *nyxtmuch* 'notmuch-args)))))
      (focus-change 'next))))

(define-command-global nyxtmuch-show (tid)
  "Show a thread in nyxtmuch."
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
