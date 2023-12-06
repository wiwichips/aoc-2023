; the second common lisp program I've ever written

; read file into list of codes
(defun read-file-lines (filename)
  "file -> list of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))
(defun get-codes () (read-file-lines "./input.txt"))
(defvar codes (get-codes))

; digits
(defvar digits
  (append 
    '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine") ; uncomment this line to get sol to part 1
    '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))

(defvar digits-values
  (mapcar (lambda (x) (list x (mod (position x digits :test #'string=) 10))) digits))

; sol
(defun digits-values-indicies (str digit-couples)
  (mapcar (lambda (x) 
    (let ((pos (search (first x) str)))
      (append x (list (if (null pos) (length str) pos))))) digit-couples))

(defun first-digit (triples)
  (let ((min-triple (first triples)))
    (dolist (triple (rest triples) (second min-triple))
      (when (< (third triple) (third min-triple))
        (setf min-triple triple)))))

(defun first-and-last (str)
  (+
    (* 10 (first-digit (digits-values-indicies str digits-values)))
    (first-digit (digits-values-indicies (reverse str) (mapcar (lambda (x) (list (reverse (first x)) (second x))) digits-values)))))

; print sol
(format t "~A~%" (reduce (lambda (x y) (format t "~A ---- ~A~%" y (first-and-last y)) (+ x (first-and-last y))) codes :initial-value 0))

