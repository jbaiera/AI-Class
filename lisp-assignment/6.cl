
(defun average-square (matrix size x y)
    (setq sum 0)
    (loop for i from x to (+ x (- size 1)) do
        (loop for j from y to (+ y (- size 1)) do
            (setq sum (+ sum (nth i (nth j matrix))))))
    (/ sum (* size size)))


