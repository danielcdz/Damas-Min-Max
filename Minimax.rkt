#lang racket
;Esteban Cruz López
(define-struct EstadoTablero (posicionesFichas))
(define-struct Nodo (hijos estadoTablero utilidad) #:transparent #:mutable ) ;Nodo estado de tablero

(define (Result raiz v) (for ([hijo (Nodo-hijos raiz)]#:when (= (Utility hijo)(Utility v))) (set! v hijo)) v)
(define (Alpha-Beta-Search nodo) (Result nodo (MAX nodo -inf.0 +inf.0)))
(define (esTerminal? estado) (empty? (Nodo-hijos estado)))

(define (MAX estado alpha beta)
    (if (esTerminal? estado) estado
        (let ([computedLevel (for/list([hijo (Nodo-hijos estado)])(MIN hijo alpha beta))])
            (define v (first computedLevel))
            (for ([minedHijo computedLevel]
                #:final (>= (Utility minedHijo) beta))
                (print (if (>= (Utility minedHijo) beta)minedHijo beta))
                (set! v (if (>(Utility minedHijo)(Utility v)) minedHijo v))
                (set-Nodo-utilidad! estado (if (>(Utility minedHijo)(Utility v)) (Utility minedHijo) (Utility v)))
                (set! alpha (max alpha (Utility v))))
            v)))

(define (MIN estado alpha beta) 
    (if (esTerminal? estado) 
        estado
        (let ([computedLevel (for/list ([hijo (Nodo-hijos estado)])(MAX hijo alpha beta))])
            (define v (first computedLevel))
            (for ([maxedHijo computedLevel]
                #:final (<= (Utility maxedHijo) alpha))
                (print (if (<= (Utility maxedHijo) alpha) maxedHijo alpha))
                (set! v (if (<(Utility maxedHijo)(Utility v)) maxedHijo v))
                (set-Nodo-utilidad! estado (if (<(Utility maxedHijo)(Utility v))(Utility maxedHijo)(Utility v)))
                (set! beta (min beta (Utility v))))
            v)))
            
(define (Utility nodo)
    (if (null? nodo) 0
    (if (= -inf.0(Nodo-utilidad nodo))
        ((lambda ()
            (set-Nodo-utilidad! nodo (EvaluarEstado (Nodo-estadoTablero nodo)))
            (Nodo-utilidad nodo)))
        (Nodo-utilidad nodo))))