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

(serapeum:export-always '*nyxtmuch*)
(defvar *nyxtmuch* nil
  "Global nyxtmuch config")

(define-command nyxtmuch-search-prompt-update (&optional (prompt-buffer (current-prompt-buffer)))
  "Update the search prompt, considering the selected suggestion.

When cursor is at \"tag:\", assume user is trying to select from one of the tags
fetched from the database. Otherwise, replace input with the selected suggestion
entirely."
  (let* ((suggestion (prompter:selected-suggestion prompt-buffer))
         (suggestion-text (prompter:attributes-default suggestion))
         (input (prompter:input prompt-buffer))
         (input-words (str:words input))
         (last-word (car (last input-words))))
      (nyxt::set-prompt-buffer-input
       (cond
        ((string-equal input suggestion-text)
         input)
        ((str:starts-with? "tag:" last-word)
         (str:unwords (append (butlast input-words) (list suggestion-text))))
        (t
         suggestion-text)))))

(define-class search-tag-source (prompter:source)
  ((prompter:name "Nyxtmuch search (supports tag search)")
   (prompter:filter-preprocessor (lambda (initial-suggestions-copy source input)
                                   (cond
                                    ((str:starts-with? "tag:" (car (last (str:words input))))
                                        ;"tag search" mode, use only the initial suggestions
                                     (prompter:delete-inexact-matches
                                      initial-suggestions-copy
                                      source
                                      (car (last (str:words input)))))
                                    (t
                                        ;otherwise, show suggestions from history
                                     (mapcar #'prompter:make-suggestion
                                             (containers:container->list
                                              ;TODO can't I get it from the prompter instead?
                                              (slot-value *nyxtmuch* 'search-history)))))))

   (prompter:filter (lambda (suggestion source input)
                        ;submatch against last word or full input, depending on cursor
                      (let ((last-word (car (last (str:words input)))))
                          (prompter:submatches
                           suggestion
                           source
                           (if (str:starts-with? "tag:" last-word)
                               last-word
                               input)))))

   (prompter:filter-postprocessor (lambda (suggestions source input)
                                    (declare (ignore source))
                                        ;input always first
                                    (append (list input) suggestions)))
   (prompter:constructor
    (lambda (s)
      (declare (ignore s))
      (let ((db-path (notmuch-config-get-database-path (slot-value *nyxtmuch* 'notmuch-args))))
          (mapcar #'(lambda (tag) (prompter:make-suggestion (str:concat "tag:" tag)))
                  (libnotmuch-get-all-tags db-path)))))))

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

;; Try to load libnotmuch
(handler-case
    (cffi:load-foreign-library "libnotmuch.so")
  (t (c)
    ;TODO should do more than just warn here
    (echo-warning "Could not load libnotmuch: ~a~%" c)))

(define-mode nyxtmuch-search-prompt-mode ()
  "mode for nyxtmuch search prompt"
  ((keymap-scheme
    (define-scheme "nm-search-prompt"
      scheme:vi-insert
      (list
       "tab" 'nyxtmuch-search-prompt-update)))))

(define-mode nyxtmuch-show-mode ()
  "mode for nyxtmuch threads"
  ((keymap-scheme
    (define-scheme "nm-show"
      scheme:vi-normal
      (list
       ;navigation
       "home" 'nyxtmuch-focus-first-message
       "end" 'nyxtmuch-focus-last-message
       "C-j" 'nyxtmuch-focus-next-message
       "C-k" 'nyxtmuch-focus-prev-message

       ;actions
       "C" 'nyxtmuch-toggle-collapse-all
       "c" 'nyxtmuch-toggle-collapse-message

       ;screen
       "r" 'nyxtmuch-render-thread)))))

(define-mode nyxtmuch-search-mode ()
  "mode for nyxtmuch searches"
  ((keymap-scheme
    (define-scheme "nm-search"
      scheme:vi-normal
      (list
       ;navigation
       "home" 'nyxtmuch-focus-first-result
       "end" 'nyxtmuch-focus-last-result
       "j" 'nyxtmuch-focus-next-result
       "k" 'nyxtmuch-focus-prev-result

       ;actions
       "return" 'nyxtmuch-show-result
       "a" 'nyxtmuch-archive-focused
       "L" 'nyxtmuch-search-tag
       "O" 'nyxtmuch-clone-search

       ;screen
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
               ("li.thread"
                :border-left "2px solid white")
               ("li.thread.selected"
                :border-left "2px solid blue"
                :background-color "#fafaff")
               ("a.threadli"
                :color "unset")
               ("div.result.unread"
                :font-weight "bold")
               ("div.tags"
                :margin "2px"
                :display "inline-block")
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
         (search-string (search-string buffer))
         (db-path (notmuch-config-get-database-path (slot-value *nyxtmuch* 'notmuch-args)))
         (search-objs (libnotmuch-lazy-search-begin search-string db-path))
         (threads-handle (first search-objs)))
      (if (not threads-handle)
          (echo-warning "Something went wrong with the lazy search.")
          (progn
           (with-current-buffer buffer
               (nyxt::html-set
                (str:concat
                 (markup:markup
                  (:style (slot-value (nyxt::make-dummy-buffer) 'style))
                  (:style style)
                  (:h1 "Nyxtmuch")
                  (:p search-string)
                  (:hr))
                 (format-search-results nil) ;silly way to get the container for search results?
                 ))
             (libnotmuch-threads-for-each
              #'(lambda (thread)
                  (let ((thread-str (format-search-result thread)))
                      ;NOTE looks like with-current-buffer is needed here to
                      ;ensure search results are added to the right pĺace --
                      ;user might move to other buffers during rendering.
                      (with-current-buffer buffer
                          (%inject-thread-result thread-str))))
              threads-handle)
             (nyxtmuch-focus-next-result))
           (libnotmuch-lazy-search-end search-objs)))))

(define-command nyxtmuch-clone-search (&optional (buffer (current-buffer)))
  "Prepare a search copying BUFFER's search terms."
  (nyxtmuch-search (slot-value buffer 'search-string)))

(define-command-global nyxtmuch-search-tag ()
  "Prepare a search starting with \"tag:\"."
  (nyxtmuch-search "tag:"))

(define-command-global nyxtmuch-search (&optional input)
  "Open nyxtmuch, possibly with preexisting search INPUT."
  (let* ((search-term
          (first (nyxt:prompt
                  :prompt "Notmuch search:"
                  :history (slot-value *nyxtmuch* 'search-history)
                  :input (or input "")
                  :sources 'search-tag-source
                  :extra-modes '(nyxtmuch-search-prompt-mode))))
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
         (thread-id (thread-id buffer))
         (thread-info (notmuch-show-single-thread
                       thread-id
                       (slot-value *nyxtmuch* 'notmuch-args))))
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
        (if thread-info
            (format-thread thread-info)
            (markup:markup (:p "Thread not found.")))))
      (when thread-info
        (nyxtmuch-focus-next-message)))))

(define-command-global nyxtmuch-show (tid)
  "Show a thread in nyxtmuch."
  (let* ((buffer-name (str:concat "*Nyxtmuch show*<thread:" tid ">"))
         (thebuf (nyxt::buffer-make *browser*
                                    :title buffer-name
                                    :buffer-class 'show-buffer)))
    (setf (slot-value thebuf 'thread-id) tid)
    (nyxtmuch-render-thread thebuf)
    (set-current-buffer thebuf)))
