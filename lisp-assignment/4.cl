
(defun min-list (xs)
    (if (= (length xs) 1) (first xs)
                          (min (first xs) (min-list (rest xs)))))

(defun max-list (xs)
    (if (= (length xs) 1) (first xs)
                          (max (first xs) (max-list (rest xs)))))

(defun median (grades)
    (if (= (length grades) 1) (first grades)
    (if (= (length grades) 2) (/ (+ (first grades) (last grades)) 2)
                              ( ))))

(defun median (grades)
    (setq sorted-grades (sort grades '<))
    (loop   (setq sorted-grades (rest (butlast sorted-grades)))
            (when (< (length sorted-grades) 3) (return
    (if (= (length sorted-grades) 1) (first sorted-grades)
    (if (= (length sorted-grades) 2) (/ (+ (first sorted-grades) (first (last sorted-grades))) 2)))))))

(defun average-grades (filename)
    (setq file-in (open '"grades.txt"))
    (setq i 1)
    (setq sum 0)
    (setq low 100)
    (setq high 0)
    (setq grades ())
    (loop   (setq i (+ i 1))
            (setq grade (read file-in))
            (setq sum (+ sum grade))
            (if (< grade low) (setq low grade) ())
            (if (> grade high) (setq high grade) ())
            (setq grades (cons grade grades))
            (when (= i 10) (return)))
    (setq average (/ sum i))
    (format t "Average: ~F~%" (coerce average 'float))
    (format t "Range:   ~D - ~D~%" low high)
    (format t "Median:  ~F" (coerce (median grades) 'float))
)

