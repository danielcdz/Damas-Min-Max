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
(define PROFUNDIDAD-IA 4)
(define CANTIDAD-JUGADORES 2)
;Una variable turno va a definir cuál es el número de jugador que tiene el turno.
(define t-inicial 1)

(define (get-ficha tablero x y)(list-ref (list-ref tablero y) x))
(define (generar-arbol-de-tableros nodo t)
    ;aquí va el código que genera el árbol de tableros.
    ; se debe recorrer el tablero preguntándo a dónde se puede mover el jugador cuyo id es el turno
    ; para cada posición en el tablero, si es un turno, generar un tablero por cada posible movimiento de esa ficha encontrada
    ; después aumentar turno y hacer lo mismo
    ; esto con un límite determinado, creo que debe ser 4
    (if (< t PROFUNDIDAD-IA);for - in range(profundidad):
        (for ([x (build-list 9 values)][y (build-list 9 values)]#:when (= (get-ficha Tablero x y) (+ (% t CANTIDAD-JUGADORES) 1)))
            (for ([posible-nuevo-estado (posibles-movimientos Tablero x y)])
                (InsertarHijo nodo posible-nuevo-estado); no importa el retorno porque voy a ir actualizando la estructura del árbol de jugadas.
                (generar-arbol-de-tableros posible-nuevo-estado (+ t 1))
            )
        )
        (if (= t t-inicial) 
            nodo 
            null
        )
    )
)
;x-1,y-1 | x,y-1 | x+1,y-1
;x-1,y   | x, y  | x+1,y
;x-1,y+1 | x,y+1 | x+1,y+1
;De este garabateo concluyo que si la ficha está en posición x,y
;se puede mover a las posiciones cuya suma no sea invertida (es decir, que a x le sume y a y le reste)
;Adicional a esto, si hay una ficha en la posición i,j se debe analizar si la puede brincar, multiplicando por 2 el salto
;Si está vacía esa otra celda y puede saltar la ficha que está cerca, esa es una posible jugada 
;pero desde ahí también puede volver a saltarse otra ficha
(define (list-copy lista)
(if (null? list) 
  '() 
  (if (list? list) 
      (cons (list-copy (car list)) (list-copy (cdr list)))
      list)))
(define (movimiento t x-ant y-ant new-x new-y)
    (if (= 0 (get-ficha t new-x new-y)))
        (mover-ficha t x y new-x new-y)
    (if (= 0 ()));Aquí se analiza si se puede saltar la ficha en new-x,new-y, porque la siguiente casilla debe estar desocupada para poder efectuar el salto
    ;También, si una vez saltado, se puede volver a saltar, se añaden las jugadas en las que salta y en las que no
)
(define (posibles-movimientos tablero x y)
    ;Para cada uno de los lugars válidos de movimiento ((x-1,y-1),(x-1,y),(x,y+1), y las demás que son con los símbolos invertidos)
    (for/list ([ñ '(-1 1)])(list 
        (movimiento tablero x y (+ x ñ) (+ y ñ) ñ)
        (movimiento tablero x y (+ x ñ) y ñ)
        (movimiento tablero x y x (+ y ñ) ñ)
    ))
)
    ;aquí va el código que, según la ficha en posición (x, y), me dice hacia dónde se puede mover en una linda lista de tableros
