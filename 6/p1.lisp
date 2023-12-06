;
; Puzzle Input
;
; Time:        56     71     79     99
; Distance:   334   1135   1350   2430
;

(defun distances (ms)
  (loop for i from 0 to ms
    collect (* i (- ms i))))

(defun ways-to-win (races distance)
  (reduce (lambda (i x) (if (> x distance) (1+ i) i)) races :initial-value 0))

(defvar leaderboard
  '((56 334) (71 1135) (79 1350) (99 2430)))

(defun per-race-ways-to-win (lb)
  (mapcar (lambda (x) (ways-to-win (distances (first x)) (second x))) lb))

(format t "~A~%" (reduce #'* (per-race-ways-to-win leaderboard)))

