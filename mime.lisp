(in-package :nx-notmuch)

(defun full-mime-type (m)
  "Return the string representation of M's mime type."
  (str:concat
   (cl-mime:content-type m) "/" (cl-mime:content-subtype m)))

(defun get-mime-content (m)
  "Return the most stringy version of mime object M's content."
  (ecase (cl-mime:content-transfer-encoding m)
    ((:8bit :7bit) (cl-mime:content m))
    ((:base64 :quoted-printable)
     (flexi-streams:octets-to-string (cl-mime:decode-content m)))))

(defun select-mime-alternative (alts)
  "Given a list of mime alternatives, prefer text/html over text/plain."
  (or (find-if (serapeum:equals "text/html")
               alts
               :key #'full-mime-type)
      (find-if (serapeum:equals "text/plain")
               (cl-mime:content alts)
               :key #'full-mime-type)))
