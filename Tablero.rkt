#lang racket

(define-struct EstadoTablero (posicionesFichas))
(define-struct Nodo (hijos estadoTablero utilidad) #:transparent #:mutable ) 
(define (esTerminal? estado) (empty? (Nodo-hijos estado)))

(define (Result raiz v) (for ([hijo (Nodo-hijos raiz)]#:when (= (Utility hijo)(Utility v))) (set! v hijo)) v)

(define (Alpha-Beta-Search nodo) (Result nodo (MAX nodo -inf.0 +inf.0)))

(define (MAX estado alpha beta)
    (if (esTerminal? estado) estado
        (let ([computedLevel (for/list([hijo (Nodo-hijos estado)])(MIN hijo alpha beta))])
            (define v (first computedLevel))
            (for ([minedHijo computedLevel]
                #:final (>= (Utility minedHijo) beta))
                (if (>= (Utility minedHijo) beta)(display (list minedHijo ">=" beta))'())
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
                (if (<= (Utility maxedHijo) alpha)(display(list maxedHijo "<=" alpha))'())
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
(define (distancia x y jugador)
  (let ([j (if (= 1 jugador)8 0)][k (if (= 1 jugador) 0 8)])
    (+ (- j x)(- k y))))
(define (EvaluarEstado tablero jugador)
  (define valor 0)
  (for ([x (build-list 9 values)][y (build-list 9 values)]#:when (= (get-ficha Tablero x y) jugador))
    (set! valor (+ valor (distancia x y jugador))))
  valor)
(define (InsertarHijo nodo nuevoEstado)
  (set-Nodo-hijos! (cons (Nodo-hijos nodo) (Nodo (list)nuevoEstado -inf.0)))
  (first (Nodo-hijos nodo)))
;En este archivo se define el tablero y el sistema de movimientos válidos según el tablero, 
;así como la calificación que tiene para cada jugador su respectivo tablero.
(define Tablero (list 
;     1 2 3 4 5 6 7 8 9
(list 0 0 0 0 0 1 1 1 1); 1
(list 0 0 0 0 0 0 1 1 1); 2
(list 0 0 0 0 0 0 0 1 1); 3
(list 0 0 0 0 0 0 0 0 1); 4
(list 0 0 0 0 0 0 0 0 0); 5
(list 2 0 0 0 0 0 0 0 0); 6
(list 2 2 0 0 0 0 0 0 0); 7
(list 2 2 2 0 0 0 0 0 0); 8
(list 2 2 2 2 0 0 0 0 0); 9
));Se inicializa con 0 en las casillas vacías y en las demás el número del jugador 
;al que pertenece la ficha en esa ubicación del tablero
(define PROFUNDIDAD-IA 4)
(define CANTIDAD-JUGADORES 2)
;Una variable turno va a definir cuál es el número de jugador que tiene el turno.
(define t-inicial 1);turno inicial al jugador 1

(define (get-ficha tablero x y)(if (or (< x 0)(< y 0)(> x 8)(> y 8)) +inf.0 (list-ref (list-ref tablero y) x)))
(define (generar-arbol-de-tableros nodo t [profundidad 0])
    (if (<= profundidad PROFUNDIDAD-IA);for _ in range(profundidad):
        (for ([x (build-list 9 values)][y (build-list 9 values)]#:when (= (get-ficha Tablero x y) t))
            (for ([posible-nuevo-estado (posibles-movimientos (Nodo-estadoTablero nodo) x y)])
                (generar-arbol-de-tableros (InsertarHijo nodo posible-nuevo-estado) t (+ profundidad 1))))
        (if (= profundidad 0) nodo null)))
(define (escoger-jugada tablero t)
  (Alpha-Beta-Search (generar-arbol-de-tableros (Nodo (list) tablero -inf.0) t)))

(define (simulador-damas-chinas)
  (mientras (no se gane)
        (set! Tablero (Nodo-estadoTablero (escoger-jugada Tablero 1)))
        (unless (jugador 1 ganó)
        (set! Tablero (Nodo-estadoTablero (escoger-jugada Tablero 2))))))

(define (distinct? v1 v2)(not (equal? v1 v2)))
(define (contains list x)
	(cond [(null? list) #f]
		[(equal? (car list) x) #t]
		[else (contains (cdr list) x)]))
(define (saltando1 x j)(+(- j x) j))
(define (posibles-movimientos tablero x y posicionesVisitadas movimientos PuedeJugarDeUno)
    (for/list ([i '(-1 1)] )
        (for/list([j (list (+ x i)(+ x i) x)]
                    [k (list (+ y i) y (+ y i))]
                    #:unless (or (contains posicionesVisitadas (list (saltando1 x j) (saltando1 y k)))
                                 (and (distinct? 0 (get-ficha tablero j k))
                                      (or (< (saltando1 x j) 0)
                                          (< (saltando1 y k) 0) 
                                          (> (saltando1 x j) 8)
                                          (> (saltando1 y k) 8))))
                    #:when (or (and PuedeJugarDeUno
                                    (= 0 (get-ficha tablero j k)))
                               (and (distinct? 0 (get-ficha tablero j k))
                                    (= 0 (get-ficha tablero (saltando1 x j) (saltando1 y k))))))
          (if (and (= 0 (get-ficha tablero j k))
                   PuedeJugarDeUno)
              (set! movimientos(append movimientos (list(copia-con-movimiento tablero x y j k))))
              (let ([ nuevoTablero (copia-con-movimiento tablero x y (saltando1 x j) (saltando1 y k)) ])
                (set! movimientos 
                      (posibles-movimientos nuevoTablero (saltando1 x j) (saltando1 y k) (append posicionesVisitadas (list(list x y)))(append movimientos(list nuevoTablero)) #f)))
              )))
  movimientos)

;x-1,y-1 | x,y-1 | x+1,y-1
;x-1,y   | x, y  | x+1,y
;x-1,y+1 | x,y+1 | x+1,y+1
;De este garabateo concluyo que si la ficha está en posición x,y
;se puede mover a las posiciones cuya suma no sea invertida (es decir, que a x le sume y a y le reste)
;Adicional a esto, si hay una ficha en la posición i,j se debe analizar si la puede brincar, multiplicando por 2 el salto
;Si está vacía esa otra celda y puede saltar la ficha que está cerca, esa es una posible jugada 
;pero desde ahí también puede volver a saltarse otra ficha
(define (list-copy list)
(if (null? list) 
  '() 
  (if (list? list) 
      (cons (list-copy (car list)) (list-copy (cdr list)))
      list)))
(define (actualizar-lista xs indice elemento)(append (take xs indice)
        (list elemento)
        (drop xs (+ indice 1))))
(define (copia-con-movimiento tablero x y j k)(let([copia (list-copy tablero)])
    (set! copia(actualizar-lista copia k (actualizar-lista (list-ref copia k) j (get-ficha tablero x y))))
    (set! copia(actualizar-lista copia y (actualizar-lista (list-ref copia y) x 0)))
    copia))
;funcion para mostrar los posibles movimientos:
(define (mostrar-movimientos movimientos)(for ([r movimientos])(for ([l r])(display l)(display "\n"))(display "\n")))
(mostrar-movimientos (posibles-movimientos Tablero 2 8 (list)(list)#t))
(display "Tablero:\n")(mostrar-movimientos (list Tablero))