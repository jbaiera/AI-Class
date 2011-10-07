
(defun averagehelper (numlist numadded sum)
    (if (= 0 (length numlist)) (/ sum numadded)
                               (averagehelper (rest numlist) (+ numadded 1) (+ sum (first numlist)))))

(defun averagelist (nums)
    (averagehelper nums 0 0))
    

