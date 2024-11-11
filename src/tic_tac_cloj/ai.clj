(ns tic-tac-cloj.ai
  (:require [tic-tac-cloj.state :as state]))

(defn get-available-moves
  "Iterate through the 'open' board positions and return a destructured array
    of available positions based on their grid coordinates."
  [board]
  (->> (for [row (range (count board))
             col (range (count (first board)))]
         (when (state/legal-move? row col board)
           [row col]))
       (remove nil?)))

(defn evaluate-position
  "This function applies a score to the potential move being evaluated. A score
    of 0 inevitably continues the game and will lead to an updated board state,
    while a 1 or -1 signal the end of the game."
  [board player]
  (cond
    (= (state/check-winner board) player) 1
    (= (state/check-winner board) (if (= player "X") "O" "X")) -1
    (empty? (get-available-moves board)) 0
    :else 0))

(defn minimax
  "Minimax is a recursive algorithm that assesses a current board-state, and
    applies scores of either nil to each one (location),indicating the game will 
    progress, or otherwise declaring a winner. If a score 1 or -1 is returned,
    a position is not returned."
  [board player depth]
  (let [winner (state/check-winner board)]
    (if (or (zero? depth)
            winner
            (empty? (get-available-moves board)))
      [(evaluate-position board player) nil]
      (let [available-moves (get-available-moves board)
            scores-and-moves (for [[row col] available-moves]
                               (let [new-board (state/update-board-state board row col player)
                                     [score _] (minimax new-board
                                                        (if (= player "X") "O" "X")
                                                        (dec depth))]
                                 [(* -1 score) [row col]]))]
        (apply max-key first scores-and-moves)))))

(defn compute-next-game-state
  [current-state input]
  (state/evaluate-and-update-game current-state
                                  (if (= (:current-player current-state) "X")
                                    input  ;Player's move
                                    (let [[_ [ai-row ai-col]] (minimax (:board current-state) "O" 9)]
                                      [ai-row ai-col]))))