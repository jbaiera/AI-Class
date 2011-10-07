
(defun print-types ()
    (loop   (setq n (read))
            (print (type-of n))
            (when (= 0 n) (return))))

