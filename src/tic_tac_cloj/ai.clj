(ns tic-tac-cloj.ai
  (:require [tic-tac-cloj.state :as state]))

(defn get-available-moves
  "Iterate through the 'open' board positions and return a destructured array
    of available positions based on their grid coordinates."
  [board]
  (for [row (range 3)
        col (range 3)
        :when (clojure.string/blank? (get-in board [row col]))]
    [row col]))

(defn evaluate-position
  "This function applies a score to the potential move being evaluated. A score
    of 1 indicates a win for the player, -1 for opponent, and 0 for ongoing/draw."
  [player winner available-moves]
  (cond
    (= winner player) 1
    (= winner (if (= player "X") "O" "X")) -1
    (empty? available-moves) 0
    :else 0))

(defn minimax
  "(minimax) is a recursive algorithm that assesses a current board-state and determines 
    the optimal move. Returns [score [move]] where score indicates the position value 
    and move is the [row col] of the best move."
  [board player depth]
  (let [winner (state/detect-winning-row board)
        available-moves (get-available-moves board)]
    (if (or (zero? depth)
            winner
            (empty? available-moves))
      [(evaluate-position player winner available-moves) nil]
      (let [scores-and-moves (for [[row col] available-moves]
                               (let [new-board (state/update-board-state board row col player)
                                     [score _] (minimax new-board
                                                        (if (= player "X") "O" "X")
                                                        (dec depth))]
                                 [(* -1 score) [row col]]))]
        (apply max-key first scores-and-moves)))))

(defn compute-next-game-state
  "This function calls the (evaluate-and-update-game) function, which will first take the 
    player's input and update the game-state with their validated input. (minimax) is called 
    to caculate and the best move for the computer to execute. These are provided to
    (evaluate-and-update-game)"
  [current-state input]
  (state/evaluate-and-update-game current-state
                                  (if (= (:current-player current-state) "X")
                                    input  ;Player's move
                                    (let [[_ [ai-row ai-col]] (minimax (:board current-state) "O" 9)]
                                      [ai-row ai-col]))))