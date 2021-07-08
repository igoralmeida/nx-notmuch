(in-package :nx-notmuch)

(define-command nyxtmuch-focus-next-message ()
  "Focus next message in show buffer."
  (focus-change 'next))

(define-command nyxtmuch-focus-prev-message ()
  "Focus prev message in show buffer."
  (focus-change 'prev))

(define-parenscript focus-change (direction)
  (defun clip (idx len)
    (max (min idx (1- len)) 0))

  (defun prev-element (current-idx collection)
    (elt collection (clip (1- current-idx) (length collection))))

  (defun next-element (current-idx collection)
    (elt collection (clip (1+ current-idx) (length collection))))

  (defun ensure-scroll (element)
    ;NOTE that scroll-into-view-if-needed is webkit-specific:
    ; https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoViewIfNeeded
    (ps:chain element (scroll-into-view-if-needed)))

  (defun update-elements ()
    (ps:let* ((messages (nyxt/ps:qsa document "li.message"))
              (old-element (nyxt/ps:qs document "li.message.selected"))
              (new-element nil)
              old-pos)
      (when old-element
        (setf (ps:@ old-element class-name) "message")
        (setf old-pos (ps:chain *array prototype index-of (call messages old-element)))
        (setf new-element (ps:lisp (case direction
                                     (prev '(prev-element old-pos messages))
                                     (next '(next-element old-pos messages))))))
      (unless new-element
        (setf new-element (elt messages 0)))
      (setf (ps:@ new-element class-name) "message selected")
      (ensure-scroll new-element)
      nil))

  (update-elements))
