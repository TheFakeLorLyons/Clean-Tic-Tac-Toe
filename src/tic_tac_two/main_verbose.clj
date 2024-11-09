;A straightforward and "clean" implementation of
;Tic-Tac-Toe
;Lorelai Lyons
;This file is a copy of the main file but commented for extra detail.

(ns tic-tac-two.main-verbose
  (:require [clojure.string :refer [blank?]]
            [tic-tac-two.console :as console]))

(def board-state
  [[" " " " " "]
   [" " " " " "]
   [" " " " " "]])

(defn legal-move? [row col board]
  (clojure.string/blank? (get-in board [row col])))

(defn check-winner
  "This part of the algorithm reviews the state of the board, iterating through
    all rows, and finally the diagnal axis of the board, determining whether 
    there are any filled lines across any one of 8 axis. If so it will return
    that particular array or nil"
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
  "Make-move functions by taking appropriate symbol ('X' or 'O') into the 
    appropriate spot in the state vectors for both the player and computer."
  [board row col player]
  (if (legal-move? row col board)
   (assoc-in board [row col] player)
   (do
     (println "Invalid move! Spot already taken.")
     board)))

(defn get-available-moves
  "Iterate through the 'open' board positions and return a destructured array
    of available positions based on their grid coordinates."
  [board-state]
  (->> (for [row (range (count board-state))
             col (range (count (first board-state)))]
         (when (legal-move? row col board-state)
           [row col]))
       (remove nil?)))

(defn evaluate-board
  "This function applies a score to the potential move being evaluated. A score
    of 0 inevitably continues the game and will lead to an updated board state,
    while a 1 or -1 signal the end of the game."
  [board player]
  (cond
    (= (check-winner board) player) 1
    (= (check-winner board) (if (= player "X") "O" "X")) -1
    (empty? (get-available-moves board)) 0
    :else 0))

(defn minimax
  "Minimax is a recursive algorithm that assesses a current board-state, and
    applies scores of either nil to each one (location),indicating the game will 
    progress, or otherwise declaring a winner. If a score 1 or -1 is returned,
    a position is not returned."
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
  [prompt valid-fn]
  (loop [] ;continuously prompt user input until valid entry
    (println prompt) ;pls
    (let [input (read-line)]
      (if (valid-fn input)
        (Integer/parseInt input)
        (do
          (println "Invalid input. Please try again.")
          (recur))))))

(defn play-game
  "This is the main game loop. Since the program is optimized/unlosable for the
    computer, technically only a draw or a loss may occur, but not by design - 
    rather the algorithm ensures that the computer plays 'optimally' so 
    theoretically the player actually could win; but the algorithm ensures at 
    least a draw as long as the computer follows the code without unforeseen 
    error. Accompanied are tests, and any further testing is welcome and I can 
    provide further testing upon request."
  [player-name stats]
  (loop [current-board board-state
         moves-made 0
         current-stats stats]
    (console/print-board current-board)
    (let [winner (check-winner current-board)]
      (cond
        winner
        (let [is-player-win? (= winner "X");loss condition or 'win' if win was possible.
              new-stats (console/update-stats current-stats (if is-player-win? :win :loss))]
          (console/print-draw-or-computer-win false is-player-win? player-name)
          (console/print-stats new-stats 0)
          (if (console/play-again?)
            (recur board-state 0 new-stats)
            new-stats))

        (>= moves-made 9);draw condition
        (let [new-stats (console/update-stats current-stats :draw)]
          (console/print-draw-or-computer-win true false player-name)
          (console/print-stats new-stats 0)
          (if (console/play-again?)
            (recur board-state 0 new-stats)
            new-stats))

        :else;if not a loss or draw, the game continues on and the computer takes a turn
        (let [row (get-valid-input "Enter your row [0, 1, 2]:" valid-input?)
              col (get-valid-input "Enter your column [0, 1, 2]:" valid-input?)
              board-after-player-move (make-move current-board row col "X")]
          (if (= board-after-player-move current-board)
            (recur current-board moves-made current-stats)
            (if (= moves-made 8)
              (recur board-after-player-move (inc moves-made) current-stats)
              (let [[_ [ai-row ai-col] :as minimax-result] (minimax board-after-player-move "O" 9)]
                (if (or (nil? ai-row) (nil? ai-col))
                  (recur board-after-player-move (inc moves-made) current-stats)
                  (let [board-after-computer-move (make-move board-after-player-move ai-row ai-col "O")]
                    (recur board-after-computer-move (+ moves-made 2) current-stats)))))))))))

(defn -main
    "The entry point to the program. It first prints a heading and then takes
      user input simply for their name. After welcoming the player, it will
      invoke the main game loop. The game will exit and print the final stats
      when the player has chosen not to continue playing."
  []
  (console/print-heading) 
  (let [player-name (read-line)
        initial-stats {:wins 0 :losses 0 :draws 0}
        _ (println (str "Okay " player-name ", welcome to the game!"))
        final-stats (play-game player-name initial-stats)]
      (console/print-stats final-stats 1)))