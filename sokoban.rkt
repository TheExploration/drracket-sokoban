;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sokoban) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

;; ~~~~~ Welcome to Sokoban ~~~~~
;; A small puzzle game where you push boxes
;;
;; Push the all boxes to the red dots to win!
;; The player is the blue circle.
;; Use the arrow keys to move around.
;; Press "r" to restart.

(@htdw Game)
;; =================
;; Constants:

(define WIDTH 360)

(define HEIGHT 280)

(define LEVEL1 (list
                (list"#""#""#""#""#""#""#""#""#")
                (list"#""#""-""-""-""-""O""-""#")
                (list"#""#"".""."".""#""#""#""#")
                (list"#""#""X""X""X""#""#""#""#")
                (list"#""#""-""-""-""-""#""#""#")
                (list"#""#""-""-""-""-""#""#""#")
                (list"#""#""#""#""#""#""#""#""#")))

(define LEVEL2 (list
                (list"#""#""#""#""#""#""#""#""#")
                (list"#""#""-""-""-""-""O""-""#")
                (list"#""#"".""."".""#""#""#""#")
                (list"#""#""X""X""X""#""#""#""#")
                (list"#""#""-""-""-""-""#""#""#")
                (list"#""#""-""-""-""-""#""#""#")
                (list"#""#""#""#""#""#""#""#""#")))

;; =================
;; Data Definitions:

(@htdd Row)
;; Row is one of:
;;  - empty
;;  - (cons String Row)
;; interp. a list of Strings
(@dd-template-rules one-of            ;2 cases
                    atomic-distinct   ;empty
                    compound          ;(cons String Row)
                    self-ref)         ;(rest row) is a list of Strings

(define (fn-for-row row)
  (cond [(empty? row) (...)]                   ;BASE CASE
        [else (... (first row)                 ;String
                   (fn-for-row (rest row)))])) ;NATURAL RECURSION




(@htdd Col)
;; Col is one of:
;;  - empty
;;  - (cons row col)
;; interp. a list of Row

(@dd-template-rules one-of            ;2 cases
                    atomic-distinct   ;empty
                    compound          ;(cons Row Col)
                    self-ref)         ;(rest Col) is a list of rows


(define (fn-for-col col)
  (cond [(empty? col) (...)]                   ;BASE CASE
        [else (... (first col)                 ;String
                   (fn-for-col (rest col)))])) ;NATURAL RECURSION

(@htdd Map)
(define-struct map (lvl1 lvl2))
;; Map is (make-map col col)
;; interp. a map with levels lvl1, lvl2
(define MAP-1 (make-map LEVEL1 LEVEL2))

(@dd-template-rules compound   ;2 fields
                    ref
                    ref)       ;(map-lvl1 m) is Col

(define (fn-for-map m)
  (... (fn-for-col (map-lvl1 m))
       (fn-for-col (map-lvl2 m))))



(@htdd Level)
;; Level is Natural
;; interp. the lvl number that is selected

(define LV1 0)
(define LV2 1)

(@dd-template-rules atomic-non-distinct) ; Natural

(define (fn-for-level l)
  (... l))

(@htdd Facing)
;; Facing is Natural
;; interp. the direction the player is facing
;; CONSTRAINT: Facing is [0, 3]
(define RIGHT 0)
(define DOWN 1)
(define LEFT 2)
(define UP 3)

(@dd-template-rules atomic-non-distinct)

(define (fn-for-facing f)
  (... f))

(@htdd Frame)
;; Frame is Natural
;; interp. the frame # for animation

(@dd-template-rules atomic-non-distinct)

(define (fn-for-frame f)
  (... f))




(@htdd Tile)
;; Tile is one of:
;; - "#"
;; - "-"
;; - "."
;; - "O"
;; - "X"
;; interp. the string that represents the tile

;; <examples are redundant for enumerations>

(@dd-template-rules one-of          ; 5 cases
                    atomic-distinct ; "#"
                    atomic-distinct ; "-"
                    atomic-distinct ; "."
                    atomic-distinct ; "O"
                    atomic-distinct); "X"
(define (fn-for-tile t)
  (cond [(string=? t "#") (...)]
        [(string=? t "-") (...)]
        [(string=? t ".") (...)]
        [(string=? t "X") (...)]
        [(string=? t "O") (...)]))


(@htdd Game)
(define-struct game (c facing frame))
;; Game is (make-game Col Facing Frame)
;; interp. a Game with Col c, player that faces towards facing, anim # frame

(define GAME (make-game 0 0 0))

(@dd-template-rules compound
                    ref
                    ref
                    ref)

(define (fn-for-game g)
  (... (fn-for-col (game-c g))
       (fn-for-facing (game-facing g))
       (fn-for-frame (game-frame g))))



;; =================
;; Functions:
(@htdf main)
(@signature Game -> Game)
;; start the world, with l as the level
;;

(@template-origin htdw-main)

(define (main l)
  (big-bang (make-game (choose-map l) 0 0) ; Level -> Game
    (to-draw render-col)                   ; Game -> Image
    (on-key update)))                      ; Game KeyEvent -> Game



(@htdf update)
(@signature Col KeyEvent -> Col)
;; Update the map

; (define (update c ke) c) ; stub

(@template-origin fn-composition)

(@template
 (define (update c ke)
   (cond [(won? c) (handle-win c)]
         [else (handle-key c ke) ...])))
          
(define (update c ke)
  (cond [(won? c) (handle-win c)]
        [else (handle-key c ke)]))






(@htdf won?)
(@signature Col -> Boolean)
;; Check if player has won

(define (won? c) false) ; stub



(@htdf handle-win)
(@signature Col -> Col)
;; Draw a Happy Face

(define (handle-win c) c) ;stub







(@htdf handle-key)
(@signature Col KeyEvent -> Col)
;; Move player based on the key input

;(define (handle-key c ke) c) ;stub

(@template-origin fn-composition)

(@template   
 (define (handle-key c ke)
   (cond [(key=? ke "right") (move-right c)]
         [(key=? ke "left") (move-left c)]
         [(key=? ke "up") (move-up c)]
         [(key=? ke "down") (move-down c)]
         [else 
          (... c)])))

(define (handle-key c ke)
  (cond [(key=? ke "right") (move-right c)]
        [(key=? ke "left") (move-left c)]
        [(key=? ke "up") (move-up c)]
        [(key=? ke "down") (move-down c)]
        [else 
         c]))





(@htdf move-right)
(@signature Col -> Col)
;; Move the player 1 Tile to the right

(define (move-right c) c)


(@htdf move-left)
(@signature Col -> Col)
;; Move the player 1 Tile to the left

(define (move-left c) c)

(@htdf move-up)
(@signature Col -> Col)
;; Move the player 1 Tile up

(define (move-up c) c)

(@htdf move-down)
(@signature Col -> Col)
;; Move the player 1 Tile down

(define (move-down c) c)



(@htdf choose-map)
(@signature Level -> Col)
;; produces the correct level
(check-expect (choose-map 0) LEVEL1)
(check-expect (choose-map 1) LEVEL2)

; (define (choose-map l) empty)

(@template-origin Level)

(@template
 (define (choose-map l)
   (... l)))

(define (choose-map l)
  (cond [(= l 0) (map-lvl1 MAP-1)]
        [(= l 1) (map-lvl2 MAP-1)]))



(@htdf render-col)
(@signature Game -> Image)
;; Produces the image for the col

; check-expect too long :(

; (define (render-col b) empty-image) ;

(@template
 (define (fn-for-col c)
   (cond [(empty? c) (...)]                   ;BASE CASE
         [else (... (first c)                 ;String
                    (fn-for-col (rest c)))])))

(define (render-col g)
  (cond [(empty? (game-c g)) empty-image]
        [else (above (render-row (first (game-c g)))
                     (render-col (rest  (game-c g))))]))
  
(@htdf render-row)
(@signature Row -> Image)
;; Produces the image for the row

; (define (render-row b2) empty-image) ;

(@template
 (define (fn-for-row r)
   (cond [(empty? r) (...)]                   ;BASE CASE
         [else (... (first r)                 ;String
                    (fn-for-row (rest r)))])))
 
(define (render-row r)
  (cond [(empty? r) empty-image]
        [else (beside (render-tile (first r))
                      (render-row (rest r)))]))

(@htdf render-tile)
(@signature Tile -> Image)
;; Produces the correct image based on the tile

; (define (render-tile t) empty-image)

(check-expect (render-tile "#")
              (overlay (rectangle 32 32 "solid" "Dim Gray")
                       (rectangle 40 40 "solid" "black")))
(check-expect (render-tile "-") (rectangle 40 40 "solid" "Papaya Whip"))

(check-expect (render-tile ".")
              (overlay (circle 8 "solid" "Dark Salmon")
                       (rectangle 40 40 "solid" "Papaya Whip")))
(check-expect (render-tile "O")
              (overlay (circle 12 "solid" "Deep Sky Blue")
                       (circle 18 "solid" "Dodger Blue")
                       (rectangle 40 40 "solid" "Papaya Whip")))

(check-expect (render-tile "X")
              (overlay
               (rectangle 32 32 "outline" "Black")
               (add-line (add-line (rectangle 32 32 "solid" "Burlywood")
                                   0 0 32 32 "Black")
                         32 0 0 32 "Black")
               (rectangle 40 40 "solid" "Tan")))

(@template
 (define (render-tile t)
   (cond [(string=? t "#") (...)]
         [(string=? ls "-") (...)]
         [(string=? ls ".") (...)]
         [(string=? ls "X") (...)]
         [(string=? ls "O") (...)])))
   
(define (render-tile t)
  (cond [(string=? t "#") (overlay (rectangle 32 32 "solid" "Dim Gray")
                                   (rectangle 40 40 "solid" "black"))]
        [(string=? t "-") (rectangle 40 40 "solid" "Papaya Whip")]
        [(string=? t ".") (overlay (circle 8 "solid" "Dark Salmon")
                                   (rectangle 40 40 "solid" "Papaya Whip"))]
        [(string=? t "O") (overlay (circle 12 "solid" "Deep Sky Blue")
                                   (circle 18 "solid" "Dodger Blue")
                                   (rectangle 40 40 "solid" "Papaya Whip"))]
        [(string=? t "X")
         (overlay
          (rectangle 32 32 "outline" "Black")
          (add-line (add-line (rectangle 32 32 "solid" "Burlywood")
                              0 0 32 32 "Black")
                    32 0 0 32 "Black")
          (rectangle 40 40 "solid" "Tan"))]
        [else empty-image]))
