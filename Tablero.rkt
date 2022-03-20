#lang racket
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
(define PROFUNDIDAD_IA 4)
(define CANTIDAD_JUGADORES 2)
;Una variable turno va a definir cuál es el número de jugador que tiene el turno.
(define t_inicial 1)

(define (get_ficha tablero x y)(list-ref (list-ref tablero y) x))
(define (generar_arbol_de_tableros nodo t)
    ;aquí va el código que genera el árbol de tableros.
    ; se debe recorrer el tablero preguntándo a dónde se puede mover el jugador cuyo id es el turno
    ; para cada posición en el tablero, si es un turno, generar un tablero por cada posible movimiento de esa ficha encontrada
    ; después aumentar turno y hacer lo mismo
    ; esto con un límite determinado, creo que debe ser 4
    (if (< t PROFUNDIDAD_IA);for _ in range(profundidad):
        (for ([x (build-list 9 values)][y (build-list 9 values)]#:when (= (get_ficha Tablero x y) (+ (% t CANTIDAD_JUGADORES) 1)))
            (for ([posible_nuevo_estado (posibles_movimientos Tablero x y)])
                (InsertarHijo nodo posible_nuevo_estado); no importa el retorno porque voy a ir actualizando la estructura del árbol de jugadas.
                (generar_arbol_de_tableros posible_nuevo_estado (+ t 1))
            )
        )
        (if (= t t_inicial) 
            nodo 
            null
        )
    )
)

(define (posibles_movimientos tablero x y)(let ([ficha (get_ficha tablero x y)])
    ;Para cada uno de los lugars válidos de movimiento ((x-1,y-1),(x-1,y),(x,y+1), y las demás que son con los símbolos invertidos)
))
    ;aquí va el código que, según la ficha en posición (x, y), me dice hacia dónde se puede mover en una linda lista de tableros
