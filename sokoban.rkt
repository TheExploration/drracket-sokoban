#lang racket

(require 2htdp/image)


;; Sokoban


;(define read (read-line (current-input-port) 'any))

(define resetBoard (list
                    (list"#""#""#""#""#""#""#""#""#")
                    (list"#""#""-""-""-""-""O""-""#")
                    (list"#""#"".""."".""#""#""#""#")
                    (list"#""#""X""X""X""#""#""#""#")
                    (list"#""#""-""-""-""-""#""#""#")
                    (list"#""#""-""-""-""-""#""#""#")
                    (list"#""#""#""#""#""#""#""#""#")))

(define Board (list
               (list"#""#""#""#""#""#""#""#""#")
               (list"#""#""-""-""-""-""O""-""#")
               (list"#""#"".""."".""#""#""#""#")
               (list"#""#""X""X""X""#""#""#""#")
               (list"#""#""-""-""-""-""#""#""#")
               (list"#""#""-""-""-""-""#""#""#")
               (list"#""#""#""#""#""#""#""#""#")))

(define printGrid 1)

(define playerY 1)

(define win false)

(define playerX 6)

(define moves 0)


;(define world (empty-scene 270 210))

(displayln "~~~~~ Welcome to Sokoban ~~~~~")
(displayln "Puzzle game where you push boxes 'X' to targets '.'! Player is 'O'")
(displayln "Controls: w = UP, a = LEFT, s = DOWN, d = RIGHT, q = QUIT, r = RESTART")
(displayln "Type in letter and press enter to move. \nOr simply leave blank and just press enter to continue movement.")



(let loop ()
  (displayln "")
  (for ([i (in-list Board)])
    (displayln i))
  (display "Input: ")
  (define read (read-line (current-input-port) 'any))
  (define move read)
  (define moveLength (string-length read))

  (loop))

(define (movePlayer str)
  (if (and (string=? str "a") (not (string=? (list-ref (list-ref Board playerX-1) playerY) "#"))))


  )


; ref(list-ref (list-ref resetBoard 0) 0)