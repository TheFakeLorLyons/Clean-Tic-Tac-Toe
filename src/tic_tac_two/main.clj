;A straightforward and "clean" implementation of
;Tic-Tac-Toe
;Lorelai Lyons

(ns tic-tac-two.main
  (:require [clojure.test :refer :all]
            [clojure.string :refer [blank?]]))

(def board-state
  [[" " " " " "]
   [" " " " " "]
   [" " " " " "]])

(defn print-board [board]
  (doseq [row board]
    (println (clojure.string/join " | " (map #(or % " ") row)))))

(defn print-heading []
  (println (str " ======================\n") 
                 "Lor's Tic-Tac-Toe Game\n"
                 "======================\n"))
;^I changed the alignment on this just so it would look
;slightly nicer... (from 'standard clojure form')

(defn legal-move? [row col board-state]
  (clojure.string/blank? (get-in board-state [row col])))

(defn check-winner
  "This part of the algorithm reviews the state of the board
   specifically by evaluating the diagnal axis of the board
   and determining whether there are any filled lines in one
   cardinal direction. If so it will return that particular 
   array or nil"
  [board]
  (let [lines (concat board
                      (apply map vector board)
                      [[(get board [0 0]) (get board [1 1]) (get board [2 2])]
                       [(get board [0 2]) (get board [1 1]) (get board [2 0])]])]
    (some (fn [line]
            (when (and (not (every? #(= " " %) line))
                       (every? #(= % (first line)) line))
              (first line)))
          lines)))

(defn make-move
  "Make-move functions by taking appropriate
   symbol ('X' or 'O') into the appropriate
   spot in the state vectors for both the
   player and computer."
  [board row col player]
  (if (blank? (get board [row col]))
    (assoc-in board [row col] player)
    (do
      (println "Invalid move! Spot already taken.")
      board)))
; ex. =>([0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1])

(defn get-available-moves
  "Iterate through the 'open' board positions and
   return a destructured array of available positions
   based on their grid coordinates."
  [board-state]
  (->> (for [row (range (count board-state))
             col (range (count (first board-state)))]
         (when (legal-move? row col board-state)
           [row col]))
       (remove nil?)))

(defn evaluate-board
  "This function applies a score to the potential move
  being evaluated. A score of 0 inevitably continues
  the game and will lead to an updated board state,
  while a 1 or -1 signal the end of the game."
  [board player]
  (cond
    (= (check-winner board) player) 1
    (= (check-winner board) (if (= player "X") "O" "X")) -1
    :else 0))

(defn minimax
  "Minimax is a well known recursive algorithm that evaluates a 
   possible set of moves and applies scores of either 0 to each 
   one,indicating the game will progress, or otherwise declaring
   a winner. In this implementation of tic-tac-toe, I have used
   scores of 1,0,-1 but sometimes it is seen as 10,0,-10."
  [board player depth]
 (if (or (zero? depth) (empty? (get-available-moves board)))
   [(evaluate-board board player) nil]
   (let [available-moves (get-available-moves board)
         scores-and-moves (for [[row col] available-moves]
                            (let [new-board (make-move board row col player)
                                  [score _] (minimax new-board
                                                     (if (= player "X") "O" "X")
                                                     (dec depth))]
                              [score [row col]]))]
     (if (= player "X")
       (reduce #(if (> (first %1) (first %2)) %1 %2) scores-and-moves)
       (reduce #(if (< (first %1) (first %2)) %1 %2) scores-and-moves)))))

(defn valid-input? [input]
  (and (not (blank? input)) (some #(= (Integer/parseInt input) %) [0 1 2])))

(defn get-valid-input
  "This function ensures that the user can only enter an integer [0 1 2]"
  [prompt valid-fn]
  (loop []
    (println prompt)
    (let [input (read-line)]
      (if (valid-fn input)
        (Integer/parseInt input)
        (do
          (println "Invalid input. Please try again.")
          (recur))))))

(defn -main
  "This is the main game loop. It first prints a heading
   and then takes user input. It will print an updated board
   each turn and continue until one of the players have one;
   correctly identifying the winner, declaring them the winner,
   and then gracefully exiting."
  []
  (let [_ (do
            (print-heading)
            (println "  Enter your REAL name"))
        player-name (read-line)
        _ (println (str "Okay " player-name ", welcome to the game!"))]
    (loop [current-board board-state] 
      (let [_ (print-board current-board)
            winner (check-winner current-board)
            winner-name (if (= winner "X") player-name
                            "Computer...")]
        (if winner
          (println (str "Player " winner-name " wins!"))
          (let [row (get-valid-input "Enter your row [0, 1, 2]:" #(valid-input? %))
                col (get-valid-input "Enter your column [0, 1, 2]:" #(valid-input? %))
                board-after-player-move (make-move current-board row col "X")
                [ai-score [ai-row ai-col]] (minimax board-after-player-move "O" 9)
                board-after-computer-move (make-move board-after-player-move ai-row ai-col "O")]
            (recur board-after-computer-move)))))))

(comment
  (defn test-minimax
    "Helper to test minimax for a given board and player"
    [board player expected-score expected-move]
    (let [[score move] (minimax board player 9)]
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

  (run-tests))