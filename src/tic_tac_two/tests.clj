(ns tic-tac-two.tests
  (:require [tic-tac-two.main :as main]
            [clojure.test :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              Mini-Max               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn test-minimax
  "Helper to test minimax for a given board and player"
  [board player expected-score expected-move]
  (let [[score move] (main/minimax board player 9)]
    (is (= score expected-score) (str "Expected score " expected-score " but got " score))
    (is (= move expected-move) (str "Expected move " expected-move " but got " move))))

       ;Test 1: Example where "X" is about to win
(test-minimax
 [["X" "O" "X"]
  [" " "X" " "]
  ["O" "O" "X"]]
 "X"
 1
 [1 1])
	  ;"X" will win by placing at (1, 1)

    ;Test 2: Example where "O" is about to win
(test-minimax
 [["X" " " " "]
  ["X" "O" "O"]
  ["O" "X" " "]]
 "X"
 -1
 [0 1])
	  ;"X" needs to block "O" by placing at (0, 1)

    ;Test 3: A draw situation (no winner)
(test-minimax
 [["X" "O" "X"]
  ["O" "X" "O"]
  ["O" "X" "O"]]
 "X"
 0
 [0 1])
	  ;No moves left, but score is 0 since it's a draw

    ;Test 4: Edge case: Board with one move left (draw)
(test-minimax
 [["X" "O" "X"]
  ["O" "X" "O"]
  ["O" "X" " "]]
 "X"
 0
 [2 2])
	  ;It's a draw but "X" plays the last move

    ;Test 5: Edge case: Player "X" must block opponent from winning
(test-minimax
 [["X" "O" " "]
  ["X" "O" " "]
  [" " "O" " "]]
 "X"
 0
 [2 0])
	  ;"X" blocks "O" at (2, 0)

    ;Test 6: Player "O" has a forced win
(test-minimax
 [["X" "O" "X"]
  ["X" "O" " "]
  ["O" "X" " "]]
 "O"
 1
 [2 2])
	  ;"O" wins by placing at (2, 2)

(testing "Minimax with invalid moves"
    ;Test: No moves left (game over)
  (test-minimax
   [["X" "O" "X"]
    ["O" "X" "O"]
    ["O" "X" "O"]]
   "X"
   0
   [0 0]))
	  ;No available moves, score is 0 because of a draw

(run-tests)