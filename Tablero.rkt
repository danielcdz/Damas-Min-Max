#lang racket
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
(define (alguien-ganó?);Esta función determina si alguien ganó el juego
  (define bandera #f)
  (for ([y (list 0 1 2 3)])
    (for ([x (for/list ([i (build-list (- 4 y) values)])(- 8 i))]
          #:when (neq? 2 (get-ficha Tablero x y)))
      (set! bandera #t)))
  (if (not bandera) 2 (let ()
      (for ([y (list 8 7 6 5)])
        (for ([x (build-list (- y 4) values)]
              #:when (neq? 1 (get-ficha Tablero x y)))
          (set! bandera #t)))
      (if (not bandera) 1 #f))))

;Definición de la estructura nodo con hijos, un tablero asociado y su valor de utilidad
(define-struct Nodo (hijos tablero utilidad dueño) #:transparent #:mutable ) 
(define (esTerminal? estado) (empty? (Nodo-hijos estado)))
(define (InsertarHijo nodo nuevoEstado dueño);Crea un nodo con el nuevo estado de tablero y se lo añade a los hijos, devuelve el hijo insertado
  (define nuevo-nodo (Nodo (list) nuevoEstado -inf.0 dueño))
  (set-Nodo-hijos! nodo (append (Nodo-hijos nodo) (list nuevo-nodo)))
  nuevo-nodo)

;La siguiente función devuelve el nodo del árbol de jugadas que tiene el tablero resultado de hacer minimax con poda alpha beta
(define (escoger-jugada tablero t)
  (Alpha-Beta-Search (generar-arbol-de-tableros (Nodo (list) tablero -inf.0 t) t)))

(define (Alpha-Beta-Search nodo) (Result (Nodo-hijos nodo) (MAX nodo -inf.0 +inf.0)))
(define (Result hijos v) 
  (if (= (Utility (first hijos))(Utility v))
      (Nodo-tablero (first hijos))
      (Result (rest hijos) v)))
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
            (set-Nodo-utilidad! nodo (EvaluarEstado (Nodo-tablero nodo) (Nodo-dueño nodo)))
            (Nodo-utilidad nodo)))
        (Nodo-utilidad nodo))))
(define (EvaluarEstado tablero jugador)
  (define valor 0)
  (for ([x (build-list 9 values)])(for([y (build-list 9 values)]#:unless(= 0 (get-ficha tablero x y)))
    (set! valor ((if (= (get-ficha tablero x y) jugador) + -) valor (distancia x y (get-ficha tablero x y))))))
  valor)
(define (distancia x y jugador)
  (+ (- (if (= 1 jugador) 8 0) x) (- (if (= 1 jugador) 0 8) y)))
(define (get-ficha tablero x y) (if (or (< x 0)(< y 0)(> x 8)(> y 8)) +inf.0
  (list-ref (list-ref tablero y) x)))

(define (generar-arbol-de-tableros nodo t [profundidad 0])
  (when (< profundidad PROFUNDIDAD-IA)
    (for ([x (list 0 1 2 3 4 5 6 7 8)])
      (for([y (list 0 1 2 3 4 5 6 7 8)]
           #:when (= (get-ficha (Nodo-tablero nodo) x y) t))
      (for ([posible-nuevo-estado (posibles-movimientos (Nodo-tablero nodo) x y)])(let ([siguiente-turno (+ (remainder t CANTIDAD-JUGADORES) 1)])
        (generar-arbol-de-tableros (InsertarHijo nodo posible-nuevo-estado siguiente-turno) siguiente-turno (+ profundidad 1)))))))
  nodo)
;La siguiente función busca y devuelve en una lista todos los estados de tablero que se pueden obtener de mover la ficha en posición x,y
;El primer parámetro condicional es para guardar los sitios visitados y que no se analicen nuevamente.
;En el segundo parámetro condicional se guardan los posibles movimientos ya calculados.
;El tercero es una bandera para que solo se puedan hacer movimientos de uno cuando no se ha hecho ningún movimiento antes.
(define (posibles-movimientos tablero x y [posicionesVisitadas '()] [movimientos '()]  [PuedeJugarDeUno #t])
    (for/list ([i '(-1 1)] );Estos fors son para obtener las coordenadas aledañas a la posición x,y que son movimientos válidos
        (for/list([j (list (+ x i)(+ x i) x)];los posibles j son: x-1, x-1, x, x+1, x+1, x
                    [k (list (+ y i) y (+ y i))];Con algo similar para los y se obtienen los lugares visitables por la ficha en posición x,y
                    #:unless (or (and (neq? 0 (get-ficha tablero j k));Cuando tiene para saltar, se analiza ese movimiento a menos de que se salga del tablero 
                                      (or (contiene? posicionesVisitadas (list (saltando1 x j) (saltando1 y k)));y a menos de que haya visitado donde 
                                          (< (saltando1 x j) 0)(< (saltando1 y k) 0)(> (saltando1 x j) 8)(> (saltando1 y k) 8))));cae cuando salta una ficha
                    #:when (or (and PuedeJugarDeUno ;Si puede jugar de un saltito y la posición del tablero está vacía
                                    (= 0 (get-ficha tablero j k)))
                               (and (neq? 0 (get-ficha tablero j k)); O si no está vacía pero se puede saltar
                                    (= 0 (get-ficha tablero (saltando1 x j) (saltando1 y k))))))
          (if (and PuedeJugarDeUno (= 0 (get-ficha tablero j k)))
              (set! movimientos(append movimientos (list(copia-con-movimiento tablero x y j k))))
              (let ([ nuevoTablero (copia-con-movimiento tablero x y (saltando1 x j) (saltando1 y k)) ])
                (set! movimientos (posibles-movimientos nuevoTablero (saltando1 x j) (saltando1 y k) (append posicionesVisitadas (list(list x y)))(append movimientos(list nuevoTablero)) #f)))
              )))
  movimientos)
      
(define (neq? v1 v2) (not (equal? v1 v2)));Definición de la función not equal para dos valores
;La siguiente función determina si la lista contiene el elemento x
(define (contiene? list x)(cond [(null? list) #f][(equal? (car list) x) #t][else (contiene? (cdr list) x)]))
(define (saltando1 x j)(+(- j x) j))
;La siguiente función devuelve una copia de la lista que recibe de parámetro
(define (list-copy list)(if (null? list)'()(if (list? list)(cons (list-copy (car list)) (list-copy (cdr list)))list)))
;La siguiente función actualiza la lista sobreescribiendo el elemento en el índice que recibe de parámetro
(define (actualizar-lista xs indice elemento)(append (take xs indice)(list elemento)(drop xs (+ indice 1))))
;La siguiente función devuelve una copia del tablero moviendo la ficha en posición x,y a la posición j,k
(define (copia-con-movimiento tablero x y j k)(let([copia (list-copy tablero)])
    (set! copia(actualizar-lista copia k (actualizar-lista (list-ref copia k) j (get-ficha tablero x y))))
    (set! copia(actualizar-lista copia y (actualizar-lista (list-ref copia y) x 0)))
    copia))
;funcion para mostrar los posibles movimientos:
(define (mostrar-movimientos movimientos)(for ([r movimientos])(for ([l r])(display l)(display "\n"))(display "\n")))
;Función que simula una partida IA vs IA
(define (simulador-damas-chinas tablero turno)
  (display "Tablero:\n")(mostrar-movimientos (list tablero))
  (sleep 2)
  (define ganador (alguien-ganó?))
  (if (not ganador)
      (simulador-damas-chinas (escoger-jugada tablero turno)  (+ (remainder turno 2) 1))
      ganador))
(simulador-damas-chinas Tablero 1)