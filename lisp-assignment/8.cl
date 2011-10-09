

(defun addPoly (polyX polyY)
    (cond 
        ((= (length (polyX)) 0) return (polyY))
        ((= (length (polyY)) 0) return (polyX))
        ((> (first (second (polyX))) (first (second (polyY))))
            (cons (first polyX) (addPoly (rest (polyX)) polyY))
        ((< (first (second (polyX))) (first (second (polyY))))
            (cons (first polyY) (addPoly polyX (rest (polyY))))
        (T 
            (cons 
                (list (+ (first(first(polyX))) (first(first(polyY)))) (first(second(polyX))))
                (addPoly (rest (polyY)) (rest (polyY)))
            )
        )
    )
