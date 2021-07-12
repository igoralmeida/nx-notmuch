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

(defun get-tag-color (tag-name canvas-color)
  "Calculates a tag color for a given TAG-NAME and background CANVAS-COLOR,
returned as (FGCOLOR . BGCOLOR).

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
