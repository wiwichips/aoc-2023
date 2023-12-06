; the first common lisp program I've ever written
(defun read-file-lines (filename)
  "file -> list of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun codes ()
  (read-file-lines "./input.txt"))

(defun codes-reversed ()
  (mapcar #'reverse (codes)))

(defun first-digit (str)
  (find-if #'digit-char-p str))

(defun first-num (str)
  (digit-char-p (first-digit str)))

(defun sum-digits (str-lister)
  (reduce #'+ (mapcar #'first-num (funcall str-lister))))

(defun main ()
  (+ (* 10 (sum-digits #'codes)) (sum-digits #'codes-reversed)))

(format t "Result: ~A~%" (main))

