;script to run after pruebas.rkt
(define p1 (Nodo (list(Nodo (list(Nodo '() '() 1)(Nodo '() '() 2))'() -inf.0)(Nodo (list (Nodo '() '() 2) (Nodo '() '() 4)) '() -inf.0)) '() -inf.0))
(define p2 (Nodo (list 
            (Nodo (list 
                (Nodo '() '() 8)
                (Nodo '() '() 8)
            ) '() 4) 
            (Nodo (list 
                (Nodo '() '() 5)
                (Nodo '() '() 7)
            ) '() 5)
        ) '() 4))
(define p3 (Nodo (list  
;MAX
    (Nodo (list             
    ;MIN
        (Nodo (list             
        ;MAX
            (Nodo (list             
            ;MIN 
                (Nodo '() '() 2)        ;MAX
                (Nodo '() '() 4)        ;MAX
            ) '() 2)             
            (Nodo (list                 ; .
                (Nodo '() '() 4)        ; .
                (Nodo '() '() 8)        ; .
            ) '() 4)
        ) '() 1)
        (Nodo (list 
            (Nodo (list 
                (Nodo '() '() 1)
                (Nodo '() '() 2)
            ) '() 1) 
            (Nodo (list 
                (Nodo '() '() 8)
                (Nodo '() '() 9)
            ) '() 8)
        ) '() 2)
    ) '()-inf.0)
    (Nodo (list
        (Nodo (list
            (Nodo (list 
                (Nodo '() '() 3)
                (Nodo '() '() 4)
            ) '() 3)
            (Nodo (list 
                (Nodo '() '() 5)
                (Nodo '() '() 9)
            ) '() 5)
        ) '() 2)
        p2
    )'() -inf.0)
)'() -inf.0))
(Alpha-Beta-Search p1)
(Alpha-Beta-Search p2)
(Alpha-Beta-Search p3)
