;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname someday-i-want-to-work-with-you) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list) ;; for shuffling/randomizing the colors of the blocks
(require 2htdp/image) ;; for creating the graphics
(require 2htdp/universe) ;; for animating

;;===========
;; constants
;;===========

;; main window dimensions
(define WINDOW-LENGHT 300)
(define WINDOW-WIDTH  640)

;; grid dimensions
(define GRID-SIDE 400)
(define ROW 8)
(define COL 8)

;; block dimensions
(define BLOCK-SIDE 20)

;; current block features
(define CURR-SIDE  (+ BLOCK-SIDE 2))
(define CURR-COLOR "cyan")

;; block horizontal separator
(define H-SEP (rectangle 20 10 "solid" "gray"))

;; blocks vertical separator
(define V-SEP (rectangle 10 250 "solid" "gray"))

;; size of the mark in a marked block
(define MARK-SIZE 15)

;; background image
(define BKGND (empty-scene WINDOW-WIDTH WINDOW-LENGHT))

;; text size
(define TXT-SIZE 24)

;; text color
(define TXT-COLOR "black")

;; grid coordinates
(define GRID-X-POS 0)
(define GRID-Y-POS 15)

;; score coordinates
(define SCORE-X-POS 0)
(define SCORE-Y-POS 15)

;; clock coordinates
(define CLOCK-X-POS 300)
(define CLOCK-Y-POS 15)

;; instructions to the player
(define arrow-inst "Use arrows to choose a block")
(define enter-inst "[Enter] to (un)select a block")
(define space-inst "[Space] to destroy selected blocks")
(define r-key-inst "Press 'R' to restart game")

;; instruction list
(define instructions (list arrow-inst enter-inst space-inst r-key-inst))

;;==================
;; data definitions
;;==================

;; color is one of 6 symbols:
;;   'r: red;
;;   'g: green;
;;   'b: blue;
;;   'y: yellow;
;;   'v: violet;
;;   'o: orange;
;;
;; interp.: it is the color of a block

;; block status is one of 2 symbols:
;;     'm: marked;
;;     'u: unmarked;
;;
;; interp.: says if a block is marked to be
;;          destroyed or not

;; block is (color status)
;; interp.: represents a block in the board/grid
;;          of the game
(define-struct block (color status))


;; row is a list of blocks with ROW elements
;; interp.: represents a row in the grid

;; grid is a list of rows
;; interp.: represents the game board
;; its indexes begin with 0 and end with
;; (ROW - 1).

;; current-block is (ROW COL)
;; inter.: the position of the current selected
;; block on the grid
;; block at (0 0) is the top left block of the grid

;; score-val is a positive integer [0, +inf)
;; I should define an upper bound for it
;; interp: represents the current score value
;;         of the player


;; sec is a positive integer [0,60)
;; interp.: represents the seconds of a clock

;; min is a positive integer [0, +inf)
;; I should define an upper bound for it
;; interp.: represents the minutes in a clock

;; clock-val is a (min sec)
;; interp.: represents the remaining time
;;          the player has
(define-struct clock-val (min sec))


;; color-gen is a list of colors
;; it is used to generate the colors of the
;; blocks in the grid
(define color-gen '(r g b y v o r g))


;; game-state is (grid curr clock-val score-val)
(define-struct game-state (grid curr time points))

;;=====================
;; function definitions
;;=====================

;; game-state -> game-state
;; start the world with:
;; (porca-miseria! (make-game-state (create-grid '()) (list 0 0) (make-clock-val 2 0) 0)) 
;; I chose "porca miseria" as a joke, I had fun doing this

(define (porca-miseria! gs)
  (big-bang gs
            (on-tick next-state 1.0)   ; game-state -> game-state
            (to-draw render)       ; game-state -> image
            (on-key  handle-key))) ; game-state key-event -> game-state

;; game-state -> game-state
;; produces the next state when no key is
;; pressed: basically makes the time runs

;(define (next-state gs) gs) ;stub

(define (next-state gs)
  (make-game-state (game-state-grid gs)
                   (game-state-curr gs)
                   (dec-clock-val (game-state-time gs))
                   (game-state-points gs)))

; game-state -> image
;; renders everything
;(define (render c) BKGND) ;stub

(define (render gs)
  (let ([score (render-score (game-state-points gs))]
        [clock (render-clock (game-state-time   gs))]
        [grid  (render-grid  (game-state-grid   gs))]
        [curr  (square CURR-SIDE "outline" CURR-COLOR)] ; this is the current block highlight
        [inst  (render-instructions instructions)])
    (place-curr curr
                (overlay-elements grid inst clock score)
                (game-state-curr gs))))


;; image1 image2 image3 image4 -> image
;; overlays image1, image2, image3, and image4
(define (overlay-elements g i c s)
  (overlay/align "left" "bottom"
                   g
                   (overlay/align "right" "bottom"
                                  i
                                  (overlay/align "right" "top"
                                                 c
                                                 (overlay/align "left" "top" 
                                                                s
                                                                BKGND)))))

;; image1 image2 curr -> image
;; places the current block highlight on the grid
;; defined by coordinates in curr
;; this function was badly coded
;; because the despair and lack of time
;; kicked in :'-(

(define (place-curr c window curr)
  (place-image c
               (+ 65 (* (second curr) 65))
               (+ 11 (* (first curr)  22))
               window))
;; game-state key-event -> game-state
;; handles the key pressed by the player
;; !!! Interesting enough I found a (somewhat) bug:
;; users should not press [Enter] and [Space] at
;; the same time because that renders 2min59sec as
;; the remaining time. I have to fix this :'-(

(define (handle-key gs k)
  (cond [(or (key=? k "up")
             (key=? k "down")
             (key=? k "left")
             (key=? k "right"))
         (make-game-state (game-state-grid gs)
                          (move-curr (game-state-curr gs) k)
                          (game-state-time gs)
                          (game-state-points gs))]
        [(key=? k " ") (make-game-state (game-state-grid gs)
                          (game-state-curr gs)
                          (inc-clock-val (game-state-time gs)
                                         (calc-sec-inc (count-explosions (game-state-grid gs))))
                          (+ (game-state-points gs)
                             (calc-score-inc (count-explosions (game-state-grid gs)))))]
        [(key=? k "\r") gs] ; I have to implement this part :'-(
        [(key=? k "r") (make-game-state (create-grid '())
                                        (list 0 0)
                                        (make-clock-val 2 0)
                                        0)]
        [else gs]))

;; Number 0 U [2,ROWxROW] -> Number
;; calculates the increment in the score
;; according to the number of matched blocks

(check-expect (calc-score-inc 0)  0)
(check-expect (calc-score-inc 2)  (floor (+ (* (- 2  1) 80) (sqr (/ (- 2  2) 5)))))
(check-expect (calc-score-inc 64) (floor (+ (* (- 64 1) 80) (sqr (/ (- 64 2) 5)))))

;(define (calc-score-inc n) 0) ;stub

; template
;(define (calc-score-inc n)
;  (... n))

(define (calc-score-inc n)
  (cond ([zero? n] n)
        (else (floor (+ (* (- n 1) 80) (sqr (/ (- n 2) 5)))))))

;; list of blocks -> number
;; counts the number of marked blocks
(define (count-marked-blocks lb)
  (cond [(empty? lb) 0]
        [else (cond [(marked? (first lb)) (count-marked-blocks (rest lb))]
                    [else (count-marked-blocks (rest lb))])]))

;; grid -> number[0,64]
;; counts the number of blocks to be exploded in the grid
(define (count-explosions g)
  (foldr + 0 (map count-marked-blocks g)))

;; ;; Number 0 U [2,ROWxROW] -> Number
;; calculates the seconds to be added in the
;; clock according to the number of matched blocks

(check-expect (calc-sec-inc 0)  0)
(check-expect (calc-sec-inc 2)  10)
(check-expect (calc-sec-inc 64) (floor (+ 10 (* (/ (sqr (- 64 2)) 3) 20))))

;(define (calc-sec-inc n) 0) ;stub

; template
;(define (calc-sec-inc n)
;  (... n))

(define (calc-sec-inc n)
  (cond ([zero? n] n)
        (else (floor (+ 10 (* (/ (sqr (- n 2)) 3) 20))))))

;; block -> block
;; toggle block status
(check-expect (toggle-status (make-block 'r 'm)) (make-block 'r 'u))
(check-expect (toggle-status (make-block 'y 'u)) (make-block 'y 'm))


;(define (toggle-status b) b) ;stub

;template
;(define (toggle-status b)
;  (... b))

(define (toggle-status b)
  (cond ([equal? 'm (block-status b)] (make-block (block-color b) 'u))
        (else (make-block (block-color b) 'm))))

;; clock-val sec -> clock-val
;; increments the clock-val by sec

(check-expect (inc-clock-val (make-clock-val 2 9) 0) (make-clock-val 2 9))
(check-expect (inc-clock-val (make-clock-val 2 9) 1) (make-clock-val 2 (+ 9 1)))
(check-expect (inc-clock-val (make-clock-val 2 9) 60)  (make-clock-val (+ 2 1) (+ 9 0)))
(check-expect (inc-clock-val (make-clock-val 2 9) 120) (make-clock-val (+ 2 2) (+ 9 0)))
(check-expect (inc-clock-val (make-clock-val 2 9) 135) (make-clock-val (+ 2 2) (+ 9 15)))

;(define (inc-clock-val c s) c) ; stub

(define (inc-clock-val c s)
  (make-clock-val (+ (clock-val-min c)
                     (quotient (+ (clock-val-sec c) s) 59))
                  (remainder (+ (clock-val-sec c) s) 60)))

;; clock-val -> clock-val
;; decrements the clock-val by 1 sec

(check-expect (dec-clock-val (make-clock-val 0 0))  (make-clock-val 0 0))
(check-expect (dec-clock-val (make-clock-val 2 0))  (make-clock-val 1 59))
(check-expect (dec-clock-val (make-clock-val 0 3))  (make-clock-val 0 2))
(check-expect (dec-clock-val (make-clock-val 2 30)) (make-clock-val 2 29))
(check-expect (dec-clock-val (make-clock-val 1 23)) (make-clock-val 1 (- 23 1)))

;(define (dec-clock-val c s) c) ; stub

(define (dec-clock-val c)
  (cond [(and (= 0 (clock-val-min c))
              (= 0 (clock-val-sec c))) c]
        [(= 0 (clock-val-min c)) (make-clock-val 0 (- (clock-val-sec c) 1))]
        [(= 0 (clock-val-sec c)) (make-clock-val (- (clock-val-min c) 1) 59)]
        [else (make-clock-val (clock-val-min c) (- (clock-val-sec c) 1))]))

;; block-color -> image-color

(define (b-color->image-color bc)
  (cond [(equal? 'r bc) "red"]
        [(equal? 'g bc) "green"]
        [(equal? 'b bc) "blue"]
        [(equal? 'y bc) "yellow"]
        [(equal? 'v bc) "violet"]
        [else "orange"]))

;; row-index column-index grid -> block
;; gives the corresponding block in position
;; row-index and column-index at the grid
(define (get-block-from-grid r c g)
  (list-ref (list-ref g c) r))

;; block -> block
;; moves the current block upward by 1,
;; if it is not yet at the grid's top bound
(check-expect (move-curr-block-up (list 0 5)) (list 0 5))
(check-expect (move-curr-block-up (list 3 5)) (list 2 5))
(check-expect (move-curr-block-up (list 3 2)) (list 2 2))
(check-expect (move-curr-block-up (list 7 2)) (list 6 2))

(define (move-curr-block-up b)
  (cond [(equal? 0 (first b)) b]
        [else (list (- (first b) 1)
                    (second b))]))

;; block -> block
;; moves the current block downward by 1,
;; if it is not yet at the grid's bottom bound
(check-expect (move-curr-block-down (list 0 5)) (list 1 5))
(check-expect (move-curr-block-down (list 3 5)) (list 4 5))
(check-expect (move-curr-block-down (list 3 2)) (list 4 2))
(check-expect (move-curr-block-down (list 7 2)) (list 7 2))

(define (move-curr-block-down b)
  (cond [(equal? (- ROW 1) (first b)) b]
        [else (list (+ (first b) 1)
                    (second b))]))

;; block -> block
;; moves the current block to the left by 1,
;; if it is not yet at the grid's left bound

(check-expect (move-curr-block-left (list 2 0)) (list 2 0))
(check-expect (move-curr-block-left (list 2 5)) (list 2 4))
(check-expect (move-curr-block-left (list 2 3)) (list 2 2))
(check-expect (move-curr-block-left (list 2 7)) (list 2 6))

(define (move-curr-block-left b)
  (cond [(equal? 0 (second b)) b]
        [else (list (first b) (- (second b) 1))]))

;; block -> block
;; moves the current block to the right by 1,
;; if it is not yet at the grid's right bound
(check-expect (move-curr-block-right (list 2 0)) (list 2 1))
(check-expect (move-curr-block-right (list 2 5)) (list 2 6))
(check-expect (move-curr-block-right (list 5 3)) (list 5 4))
(check-expect (move-curr-block-right (list 2 7)) (list 2 7))

(define (move-curr-block-right b)
  (cond [(equal? (- COL 1) (second b)) b]
        [else (list (first b) (+ (second b) 1))]))

;; curr-block key-event -> curr-block
;; produces the new current block position
;; according to the arrow key pressed
(define (move-curr cb k)
  (cond [(key=? k "up")   (move-curr-block-up cb)]
        [(key=? k "down") (move-curr-block-down cb)]
        [(key=? k "left") (move-curr-block-left cb)]
        [else (move-curr-block-right cb)]))

;; score-val -> image
;; renders the value of the player's current score
(check-expect (render-score 0)   (text "Score: 0"   TXT-SIZE TXT-COLOR))
(check-expect (render-score 1)   (text "Score: 1"   TXT-SIZE TXT-COLOR))
(check-expect (render-score 250) (text "Score: 250" TXT-SIZE TXT-COLOR))

(define (render-score s)
  (text (string-append "Score: "
                       (number->string s)) TXT-SIZE TXT-COLOR))

;; clock-val -> image
;; renders the image of the countdown clock

(check-expect (render-clock (make-clock-val 0 0))  (text "Remaining time 0:00" TXT-SIZE TXT-COLOR))
(check-expect (render-clock (make-clock-val 1 2))  (text "Remaining time 1:02" TXT-SIZE TXT-COLOR))
(check-expect (render-clock (make-clock-val 2 15)) (text "Remaining time 2:15" TXT-SIZE TXT-COLOR))

(define (render-clock c)
  (text (string-append "Remaining time "
                       (clock-val->string c)) TXT-SIZE TXT-COLOR))

;; clock-val -> string
;; creates a string out of a clock-val

(check-expect (clock-val->string (make-clock-val 0 0))  "0:00")
(check-expect (clock-val->string (make-clock-val 1 2))  "1:02")
(check-expect (clock-val->string (make-clock-val 5 15)) "5:15")

(define (clock-val->string c)
  (cond [(> 10 (clock-val-sec c)) (string-append (number->string (clock-val-min c)) ":"
                                                 (string-append "0"
                                                                (number->string (clock-val-sec c))))]
        [else (string-append (number->string (clock-val-min c))
                             ":"
                             (number->string (clock-val-sec c)))]))


;; list of instructions -> image
;; renders the list of instructions
;; sorrounded by a box

(define (render-instructions l)
  (cond [(empty? l) (text "" TXT-SIZE TXT-COLOR)]
        [else (overlay/align/offset "left" "top"
                                    (text (first l) TXT-SIZE TXT-COLOR)
                                    0 28
                                    (render-instructions (rest l)))]))

;; block -> boolean
;; true if the status of a block is marked, and
;; false otherwise
(check-expect (marked? (make-block 'y 'm)) #t)
(check-expect (marked? (make-block 'y 'u)) #f)

(define (marked? b)
  (cond [(equal? (block-status b) 'm) #t]
        [else #f]))

;; block -> image
;; renders a block

(define (render-block b)
  (cond [(marked? b) (overlay (text "X" MARK-SIZE "white")
                              (square BLOCK-SIDE
                                      "solid"
                                      (b-color->image-color (block-color b))))]
        [else (square BLOCK-SIDE "solid" (b-color->image-color (block-color b)))]))

;; block-color -> block
;; creates an unmarked block with the given color
(check-expect (create-unmarked-block 'b) (make-block 'b 'u))

(define (create-unmarked-block c)
  (make-block c 'u))

;; list of blocks -> list of blocks
;; produces a list of COL unmarked blocks with the
;; available colors in the palette
;; color distribution should be reasonably random
(define (list-of-blocks l)
  (cond [(empty? l) '()]
        [else (cons (create-unmarked-block (first l))
                    (list-of-blocks (rest l)))]))


;; grid -> grid
;; creates a grid with COL x ROW unmarkerd blocks
;; input g must be empty list
(define (create-grid g)
  (helper-create-grid g 0))

;; grid number -> grid

(define (helper-create-grid g n)
  (cond [(= n COL) '()]
        [else (cons (shuffle (list-of-blocks color-gen))
                    (helper-create-grid g (+ n 1)))]))

;; grid -> image
;; renders the grid

(define (render-grid g)
  (render-grid-columns g))

;; grid -> image
;; renders each column of the given grid
(define (render-grid-columns g)
  (cond [(empty? g) (empty-scene 0 0)]
        [else (beside (render-grid-rows (first g))
                      V-SEP
                      (render-grid-columns (rest g)))]))

;; list of blocks -> image
;; renders (somewhat) the rows of a given column of
;; the grid
(define (render-grid-rows l)
  (cond [(empty? l) (empty-scene 0 0)]
        [else (above (render-block (first l))
                     H-SEP
                     (render-grid-rows (rest l)))]))

;; block -> block
;; marks an unmarked block it
;; the player is allowed to do so
;; !!!
(define (mark-block b) b) ; stub

;; grid -> grid
;; substitute the marked blocks in the given grid
;; to new "random" unmarked blocks
;; !!!
(define (explode-blocks g) g) ;stub

;; test image
(define img (render (make-game-state (create-grid '()) (list 0 0) (make-clock-val 2 0) 0)))