

(defun addPoly (polyX polyY)
    (cond 
        ((= (length (polyX)) 0) return (polyY))
        ((= (length (polyY)) 0) return (polyX))
        ((> (second (first (polyX))) (second (first (polyY))))
            (cons (first polyX) (addPoly (rest (polyX)) polyY)))
        ((< (second (first (polyX))) (second (first (polyY))))
            (cons (first polyY) (addPoly polyX (rest (polyY)))))
        (T 
            (cons 
                (list (+ (first(first(polyX))) (first(first(polyY)))) (second(first(polyX))))
                (addPoly (rest (polyY)) (rest (polyY)))
            )
        )
    )
)
