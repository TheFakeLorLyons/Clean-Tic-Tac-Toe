;A straightforward and "clean" implementation of
;Tic-Tac-Toe
;Lorelai Lyons

(ns tic-tac-two.main
  (:require [clojure.string :refer [blank?]]))

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

(defn legal-move? [row col board]
  (clojure.string/blank? (get-in board [row col])))

(defn check-winner
  "This part of the algorithm reviews the state of the board
   specifically by evaluating the diagnal axis of the board
   and determining whether there are any filled lines in one
   cardinal direction. If so it will return that particular 
   array or nil"
  [board]
  (let [diag1 [(get-in board [0 0]) (get-in board [1 1]) (get-in board [2 2])]
        diag2 [(get-in board [0 2]) (get-in board [1 1]) (get-in board [2 0])]
        rows board
        cols (apply map vector board)
        all-lines (concat [diag1 diag2] rows cols)]
    (some (fn [line]
            (when (and (not= " " (first line))
                       (apply = line))
              (first line)))
          all-lines)))

(defn make-move
  "Make-move functions by taking appropriate
   symbol ('X' or 'O') into the appropriate
   spot in the state vectors for both the
   player and computer."
  [board row col player]
  (if (legal-move? row col board)
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
    (empty? (get-available-moves board)) 0
    :else 0))

(defn minimax
  "Minimax is a well known recursive algorithm that evaluates a 
   possible set of moves and applies scores of either 0 to each 
   one,indicating the game will progress, or otherwise declaring
   a winner. In this implementation of tic-tac-toe, I have used
   scores of 1,0,-1 but sometimes it is seen as 10,0,-10."
  [board player depth]
  (let [winner (check-winner board)]
   (if (or (zero? depth)
           winner
           (empty? (get-available-moves board)))
     [(evaluate-board board player) nil]
     (let [available-moves (get-available-moves board)
           scores-and-moves (for [[row col] available-moves]
                              (let [new-board (make-move board row col player)
                                    [score _] (minimax new-board
                                                       (if (= player "X") "O" "X")
                                                       (dec depth))]
                                [(* -1 score) [row col]]))]
       (apply max-key first scores-and-moves)))))

(defn valid-input? [input]
  (try
   (and (not (blank? input))
        (some #(= (Integer/parseInt input) %) [0 1 2]))
   (catch NumberFormatException _ false)))

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
    (loop [current-board board-state
           moves-made 0]
      (print-board current-board)
      (let [winner (check-winner current-board)]
        (cond
          winner
          (println (str "Player " (if (= winner "X") player-name "Computer") " wins!"))

          (>= moves-made 9)
          (println "Game ends in a draw!")
          
          :else
          (let [row (get-valid-input "Enter your row [0, 1, 2]:" valid-input?)
                col (get-valid-input "Enter your column [0, 1, 2]:" valid-input?)
                board-after-player-move (make-move current-board row col "X")]
            (if (= board-after-player-move current-board)
              (recur current-board moves-made)
              (if (= moves-made 8)
                (recur board-after-player-move (inc moves-made))
                (let [[_ [ai-row ai-col] :as minimax-result] (minimax board-after-player-move "O" 9)]
                  (if (or (nil? ai-row) (nil? ai-col))
                    (recur board-after-player-move (inc moves-made))
                    (let [board-after-computer-move (make-move board-after-player-move ai-row ai-col "O")]
                      (recur board-after-computer-move (+ moves-made 2)))))))))))))