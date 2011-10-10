
(defun average-square (matrix size x y)
    (setq sum 0)
    (loop for i from x to (+ x (- size 1)) do
        (loop for j from y to (+ y (- size 1)) do
            (setq sum (+ sum (nth i (nth j matrix))))))
    (/ sum (* size size)))

(defun average-matrix (matrix)
    (setq newmatrix ())
    (loop for i from 0 to 3 do
        (setq row ())
        (loop for j from 0 to 3 do
            (setq row (cons (average-square matrix 4 (* i 4) (* j 4)) row))
            )
        (setq newmatrix (cons row newmatrix)))
    (newmatrix))

