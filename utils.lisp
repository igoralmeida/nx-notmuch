(in-package :nx-notmuch)

(defun get-tag-color (tag-name canvas-color)
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
