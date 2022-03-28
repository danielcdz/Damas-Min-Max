
#lang racket/gui

(require embedded-gui)

;; Super clase de las fichas
(define chess-piece-snip-class
  (make-object
   (class snip-class%
     (super-new)
     (send this set-classname "chess-piece-snip"))))

(send (get-the-snip-class-list) add chess-piece-snip-class)
;; -----------------------------------------------------------


;; Clase de la pieza
(define chess-piece%
  (class snip%
    (init-field glyph font size [location #f])
    (super-new)
    (send this set-snipclass chess-piece-snip-class)

    (define/public (set-location l) (set! location l))
    (define/public (get-location) location)

    (define/override (get-extent dc x y width height descent space lspace rspace)
      (when width (set-box! width size))
      (when height (set-box! height size))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    (define/override (draw dc x y . other)
      (send dc set-font font)
      (send dc set-text-foreground "black")
      (define-values (glyph-width glyph-height baseline extra-space)
        (send dc get-text-extent glyph font #t))
      (let ((ox (/ (- size glyph-width) 2))
            (oy (/ (- size glyph-height 2))))
        (send dc draw-text glyph (+ x ox) (+ y oy))))
    ))
;; -------------------------------------------------------------

;; datos unicode clave-valor de cada una de las fichas
(define chess-piece-data
  (hash
   "A" #\u2727 "x" #\u0000
  "b" #\u2726 ))
;; -------------------------------------------------------------


;; Funcion para la creacion de las fichas 
(define (make-chess-piece id [location #f])
  (define glyph (hash-ref chess-piece-data id))
  (define font (send the-font-list find-or-create-font 25 'default 'normal 'normal))
  (new chess-piece% [glyph (string glyph)] [font font] [size 35] [location location]))
;; -------------------------------------------------------------


;; Clase tablero
(define chess-board%
  (class pasteboard%
    (super-new)

    (define/override (on-paint before? dc . other)
      (when before?
        (draw-chess-board dc)))

    (define/augment (after-insert chess-piece . rest)
      (position-piece this chess-piece))
    
    (define/augment (on-display-size)
      (send this begin-edit-sequence)
      (let loop ([snip (send this find-first-snip)])
        (when snip
          ;; Reposition the piece, since the location is stored as text
          ;; (e.g. d3) its new coordinates will be recomputed to the correct
          ;; place
          (position-piece this snip)
          (loop (send snip next))))
      (send this end-edit-sequence))

    ))
;; ------------------------------------------------------------



(define (position-piece board piece)
  (define-values (canvas-width canvas-height)
    (let ((c (send board get-canvas)))
      (send c get-size)))
  (define-values (square-width square-height)
    (values (/ canvas-width 9) (/ canvas-height 9)))
  (define-values (rank file)
    (location->rank-file (send piece get-location)))
  (define-values (square-x square-y)
    (values (* file square-width) (* rank square-height)))
  (define piece-width (snip-width piece))
  (define piece-height (snip-height piece))
  (send board move-to piece
        (+ square-x (/ (- square-width piece-width) 2))
        (+ square-y (/ (- square-height piece-height) 2))))



;; Funcion para definir las coordenadas en el tablero
(define (location->rank-file location)
  (unless (and (string? location) (= (string-length location) 2))
    (raise-argument-error 'location "valid  position a1 .. h8" location))
  (define file
    (index-of '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8) (string-ref location 0)))
  (define rank
    (index-of '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8) (string-ref location 1)))
  (unless (and rank file)
    (raise-argument-error 'location "valid  position a1 .. h8" location))
  (values rank file))
;; ------------------------------------------------------------


;; Funcion para dibujar el tablero
(define (draw-chess-board dc)
  (define brush (send the-brush-list find-or-create-brush "skyblue" 'solid))
  (define pen (send the-pen-list find-or-create-pen "black" 2 'solid))
  (define font (send the-font-list find-or-create-font 9 'default 'normal 'normal))
  (define-values (dc-width dc-height) (send dc get-size))
  (define cell-width (/ dc-width 9))
  (define cell-height (/ dc-height 9))
  (define margin 3)
    
  (send dc clear)
  (send dc set-brush brush)
  (send dc set-pen pen)
  (send dc set-font font)
  
  (for* ([row (in-range 9)] [col (in-range 9)]
         #:when (or (and (odd? row) (even? col))
                    (and (even? row) (odd? col))))
    (define-values [x y] (values (* col cell-width) (* row cell-height)))
    (send dc draw-rectangle x y cell-width cell-height))

 ;; (for ([(rank index) (in-indexed '("9" "8" "7" "6" "5" "4" "3" "2" "1"))])
  ;;  (define-values [_0 h _1 _2] (send dc get-text-extent rank font #t))
  ;;  (define y (+ (* index cell-height) (- (/ cell-height 2) (/ h 2))))
  ;;  (send dc draw-text rank margin y))
  
 ;; (for ([(file index) (in-indexed '("a" "b" "c" "d" "e" "f" "g" "h" "i"))])
  ;;  (define-values [w h _1 _2] (send dc get-text-extent file font #t))
  ;;  (define x (+ (* index cell-width) (- (/ cell-width 2) (/ w 2))))
   ;; (send dc draw-text file x (- dc-height h margin))))
)
;; -------------------------------------------------------------



;; A test program for our chess-piece% objects:

;; The pasteboard% that will hold and manage the chess pieces
(define board (new chess-board%))
;; Toplevel window for our application
(define toplevel (new frame% [label "Damas chinas"] [width (* 50 8)] [height (* 50 8)]))
;; The canvas which will display the pasteboard contents
(define canvas (new editor-canvas%
                    [parent toplevel]
                    [style '(no-hscroll no-vscroll transparent)]
                    [horizontal-inset 0]
                    [vertical-inset 0]
                    [editor board]))
(send toplevel show #t)

;; Variable de posiciones iniciales del tablero
(define initial
  (string-append
   "A80A81A82A83A70A71A72A60A61A50"
   "b08b18b28b38b07b17b27b06b16b05"))


;; Funcion para iniciar el tablero y cada una de sus fichas
(define (setup-board board position)
  (send board clear)
  (define piece-count (/ (string-length position) 3))
  (for ([index (in-range piece-count)])
    (define pos (* index 3))
    (define name (substring position pos (add1 pos)))
    (define location (substring position (add1 pos) (+ (add1 pos) 2)))
    (send board insert (make-chess-piece name location))))

(define Tablero (list 
;     1 2 3 4 5 6 7 8 9
(list 0 0 0 0 0 1 1 1 1); 1
(list 0 0 0 0 0 0 1 1 1); 2
(list 0 0 2 0 0 0 0 1 1); 3
(list 0 0 0 0 0 0 0 0 1); 4
(list 0 0 0 0 1 0 0 0 0); 5
(list 2 0 0 0 0 0 0 0 0); 6
(list 2 2 0 0 0 0 0 0 0); 7
(list 2 2 2 0 0 0 0 0 0); 8
(list 2 2 2 2 0 0 2 0 0); 9
))



(define (tableroToString tablero)
  (define res "")
  (define ficha1 "A")
  (define ficha2 "b")
     (for* ([x (build-list 9 values)][y (build-list 9 values)])
       (define dato ( list-ref( list-ref tablero y) x))

       (if (equal? dato 1) (set! res(string-append res (string-append "A" (string-append (format "~v" x) (format "~v" y))) ))  1)
       (if (equal? dato 2) (set! res(string-append res (string-append "b" (string-append (format "~v" x) (format "~v" y))) ))  2)

       )(print res)
  res
  )







(define tablero2 (list 
                  ;     1 2 3 4 5 6 7 8 9
                  (list 0 0 0 0 0 1 1 1 1); 1
                  (list 0 0 0 0 0 0 0 1 1); 2
                  (list 0 0 0 1 0 0 0 0 1); 3
                  (list 0 0 0 0 0 1 0 0 1); 4
                  (list 0 0 0 0 0 0 0 0 0); 5
                  (list 2 0 0 0 0 0 0 0 0); 6
                  (list 2 0 0 0 0 2 0 0 0); 7
                  (list 2 2 0 0 0 2 0 0 0); 8
                  (list 2 2 2 2 0 0 0 0 0); 9
                  ))
(tableroToString Tablero)
(setup-board board (tableroToString Tablero))

(send board erase) ; instruccion para borrar todas las fichas
(tableroToString tablero2)
(setup-board board (tableroToString tablero2))



