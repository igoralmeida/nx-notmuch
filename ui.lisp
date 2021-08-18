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

;;; simple commands

;; show

(define-command nyxtmuch-focus-next-message ()
  "Focus next message in show buffer."
  (focus-change 'next "li" "message"))

(define-command nyxtmuch-focus-prev-message ()
  "Focus prev message in show buffer."
  (focus-change 'prev "li" "message"))

(define-command nyxtmuch-focus-first-message ()
  "Focus first thread in show buffer."
  (focus-change 'first "li" "message"))

(define-command nyxtmuch-focus-last-message ()
  "Focus last thread in show buffer."
  (focus-change 'last "li" "message"))

;; search

(define-command nyxtmuch-focus-next-result ()
  "Focus next thread in search buffer."
  (focus-change 'next "li" "thread"))

(define-command nyxtmuch-focus-prev-result ()
  "Focus previous thread in search buffer."
  (focus-change 'prev "li" "thread"))

(define-command nyxtmuch-focus-first-result ()
  "Focus first thread in search buffer."
  (focus-change 'first "li" "thread"))

(define-command nyxtmuch-focus-last-result ()
  "Focus last thread in search buffer."
  (focus-change 'last "li" "thread"))

(define-command nyxtmuch-show-result ()
  "From the search buffer, open a show buffer with the focused thread."
  ;NOTE This should do essentially what nyxt's follow-hint does. Not sure if
  ;there's a way to reuse that without recreating the hint object here.
  (let ((cmd (get-thread-anchor-code (%get-thread-anchor-href))))
    (when (and (eq (length cmd) 2)
               (eq (first cmd) 'nyxtmuch-show)
               (typep (second cmd) 'string))
      (eval cmd))))

;;; utils

;; A focus changer for both search and show buffers, works by adding/removing
;; the `selected' class from elements of tag HTML-TAG and class HTML-TAG-CLASS
;;
;; DIRECTION is 'prev 'next 'first or 'last
;; HTML-TAG is a string like "li" or "div"
;; HTML-TAG-CLASS is the main class like "message" or "thread"
;;
;; Seems kind of expensive to enumerate all the candidates every time, but there
;; you go. At least trees are easily traversed like this...
(define-parenscript focus-change (direction html-tag html-tag-class)
  (defun clip (idx len)
    (max (min idx (1- len)) 0))

  (defun prev-element (current-idx collection)
    (elt collection (clip (1- current-idx) (length collection))))

  (defun next-element (current-idx collection)
    (elt collection (clip (1+ current-idx) (length collection))))

  (defun first-element (current-idx collection)
    (elt collection 0))

  (defun last-element (current-idx collection)
    (elt collection (1- (length collection))))

  (defun ensure-scroll (element)
    ;NOTE that scroll-into-view-if-needed is webkit-specific:
    ; https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoViewIfNeeded
    (ps:chain element (scroll-into-view-if-needed)))

  (defun update-elements ()
    (ps:let* ((search-term (ps:lisp (str:join "." `(,html-tag ,html-tag-class))))
              (marked-term (ps:lisp (str:join "." `(,html-tag ,html-tag-class "selected"))))
              (candidates (nyxt/ps:qsa document search-term))
              (old-element (nyxt/ps:qs document marked-term))
              (new-element nil)
              old-pos)
      (when old-element
        (setf (ps:@ old-element class-name) (ps:lisp html-tag-class))
        (setf old-pos (ps:chain *array prototype index-of (call candidates old-element)))
        (setf new-element (ps:lisp (case direction
                                     (prev '(prev-element old-pos candidates))
                                     (next '(next-element old-pos candidates))
                                     (first '(first-element old-pos candidates))
                                     (last '(last-element old-pos candidates))))))
      (unless new-element
        (setf new-element (elt candidates 0)))
      (setf (ps:@ new-element class-name) (ps:lisp (str:concat html-tag-class " selected")))
      (ensure-scroll new-element)
      nil))

  (update-elements))

(defun get-thread-anchor-code (href)
  "Return the lisp code in HREF, a string like 'lisp://(cmd args)'."
  (let ((url-code (quri:url-decode (nyxt::schemeless-url (quri:uri href)))))
    (with-input-from-string (input url-code)
      (read input nil :eof))))

(define-parenscript %get-thread-anchor-href ()
  "Return the href (a lisp:// code) of the selected result in the search
buffer."
  (ps:let* ((thread (nyxt/ps:qs document "li.thread.selected"))
            thread-link)
    (when thread
      (setf thread-link (nyxt/ps:qs thread "a.threadli"))
      (when thread-link
        (ps:@ thread-link href)))))

(define-parenscript %inject-thread-result (thread-html-str)
  "Inject THREAD-HTML-STR as another child of the thread list."
  (ps:let ((thread-list (nyxt/ps:qs document "#threadlist"))
            new-thread)
    (when thread-list
      (setf new-thread (ps:chain document (create-element nil)))
      (ps:chain thread-list (append-child new-thread))
      (setf (ps:@ new-thread outer-h-t-m-l) (ps:lisp thread-html-str))
      nil)))

;;; show commands

(define-parenscript nyxtmuch-toggle-collapse-message ()
  "Hide the body of the selected message in the show buffer."
  (defun ensure-scroll (element)
    ;TODO why do i have to repeat this defun here?

    ;NOTE that scroll-into-view-if-needed is webkit-specific:
    ; https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoViewIfNeeded
    (ps:chain element (scroll-into-view-if-needed)))

  (ps:let* ((message (nyxt/ps:qs document "li.message.selected")))
    (when message
      (let* ((bodydiv (nyxt/ps:qs message "div.messagebody")))
        (if (ps:equal (ps:chain bodydiv style display) "none")
            (setf (ps:chain bodydiv style display) nil)
            (setf (ps:chain bodydiv style display) "none")))
      (ensure-scroll message))
    nil))

;;; search commands

(define-command nyxtmuch-archive-focused ()
  "Toggle the `inbox' tag of the currently selected thread in the search buffer.

Refreshes the buffer afterwards, which is not ideal. TODO"
  (let* ((thread-id (second (get-thread-anchor-code (%get-thread-anchor-href))))
         (notmuch-args (slot-value *nyxtmuch* 'notmuch-args))
         (tags (notmuch-get-tags thread-id notmuch-args)))
    (when thread-id
      (notmuch-tag-thread
       thread-id
       (list
        (format nil "~:[+~;-~]inbox" (member "inbox" tags :test #'equal)))
       notmuch-args)
      (nyxtmuch-render-search))))

