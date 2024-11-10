(ns tic-tac-cloj.main
  (:require [clojure.string :refer [blank?]]
            [tic-tac-cloj.console :as console]))

(def board-state
  [[" " " " " "]
   [" " " " " "]
   [" " " " " "]])

(defn legal-move? [row col board]
  (clojure.string/blank? (get-in board [row col])))

(defn check-winner
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
  [board row col player]
  (if (legal-move? row col board)
   (assoc-in board [row col] player)
   (do
     (println "Invalid move! Spot already taken.")
     board)))

(defn get-available-moves
  [board]
  (->> (for [row (range (count board))
             col (range (count (first board)))]
         (when (legal-move? row col board)
           [row col]))
       (remove nil?)))

(defn evaluate-board
  [board player]
  (cond
    (= (check-winner board) player) 1
    (= (check-winner board) (if (= player "X") "O" "X")) -1
    (empty? (get-available-moves board)) 0
    :else 0))

(defn minimax
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

(defn handle-player-move
  [current-board moves-made current-stats]
  (let [row (get-valid-input "Enter your row [0, 1, 2]:" valid-input?)
        col (get-valid-input "Enter your column [0, 1, 2]:" valid-input?)
        board-after-player-move (make-move current-board row col "X")]
    (if (= board-after-player-move current-board)
      (recur current-board moves-made current-stats)
      [board-after-player-move (inc moves-made) current-stats])))

(defn handle-computer-move
  [board-after-player-move moves-made current-stats]
  (if (>= moves-made 9)
    [board-after-player-move moves-made current-stats]
    (let [[_ [ai-row ai-col] :as minimax-result] (minimax board-after-player-move "O" 9)]
      (if (or (nil? ai-row) (nil? ai-col))
        [board-after-player-move moves-made current-stats]
        (let [board-after-computer-move (make-move board-after-player-move ai-row ai-col "O")]
          [board-after-computer-move (inc moves-made) current-stats])))))

(defn play-game
  [player-name stats]
  (loop [current-board board-state
         moves-made 0
         current-stats stats]
    (console/print-board current-board)
    (let [winner (check-winner current-board)]
      (cond
        winner
        (let [is-player-win? (= winner "X");loss condition or 'win' if win was possible.
              [new-stats playing-again?] (console/handle-game-over current-board current-stats winner moves-made is-player-win? player-name)]
          (if playing-again?
            (recur board-state 0 new-stats)
            new-stats))

        (>= moves-made 9);draw condition
        (let [[new-stats playing-again?] (console/handle-game-over current-board current-stats nil moves-made false player-name)]
          (if playing-again?
            (recur board-state 0 new-stats)
            new-stats))

        :else            ;if not a loss or draw, the game continues on and the computer takes a turn
        (let [[board-after-player-move new-move-count updated-stats] (handle-player-move current-board moves-made current-stats)
              [board-after-computer-move final-move-count final-stats] (handle-computer-move board-after-player-move new-move-count updated-stats)]
          (recur board-after-computer-move final-move-count final-stats))))))

(defn -main
  []
  (console/print-heading) 
  (let [player-name (read-line)
        initial-stats {:wins 0 :losses 0 :draws 0}
        _ (println (str "Okay " player-name ", welcome to the game!"))
        final-stats (play-game player-name initial-stats)]
      (console/print-stats final-stats 1)))