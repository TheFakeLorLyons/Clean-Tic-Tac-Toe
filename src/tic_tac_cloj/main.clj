(ns tic-tac-cloj.main
  (:require [clojure.string :refer [blank?]]
            [tic-tac-cloj.console :as console]))

(def board-state
  {:board [[" " " " " "]
           [" " " " " "]
           [" " " " " "]]
   :moves-made 0
   :stats {:wins 0 :losses 0 :draws 0}
   :player-name nil
   :current-player "X"})

(def game-state (atom board-state))

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
    board))

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

(defn game-over-state [current-state]
  (let [{:keys [board moves-made]} current-state
        winner (check-winner board)]
    (cond
      winner
      {:state :game-over
       :next-state (-> current-state
                      (assoc :winner winner)
                      (update :stats #(update % (if (= winner "X") :wins :losses) inc)))}

      (>= moves-made 9)
      {:state :game-over
       :next-state (-> current-state
                      (assoc :winner nil)
                      (update :stats #(update % :draws inc)))}

      :else nil)))
(defn handle-move
  [current-state [row col]]
  (let [{:keys [board current-player]} current-state
        new-board (make-move board row col current-player)]
    (if (= new-board board)
      {:state :invalid-move
       :next-state current-state}
      {:state :continue
       :next-state (-> current-state
                       (assoc :board new-board)
                       (update :moves-made inc)
                       (assoc :current-player (if (= current-player "X") "O" "X")))})))

(defn compute-next-state
  [current-state input]
  (handle-move current-state
               (if (= (:current-player current-state) "X")
                 input  ; Player's move
                 (let [[_ [ai-row ai-col]] (minimax (:board current-state) "O" 9)]
                   [ai-row ai-col]))))

(defn handle-player-turn []
  (let [row (get-valid-input "Enter your row [0, 1, 2]:" valid-input?)
        col (get-valid-input "Enter your column [0, 1, 2]:" valid-input?)]
    [row col]))

(defn play-game [player-name]
  (reset! game-state (assoc board-state :player-name player-name))
  (loop []
    (let [current-state @game-state]
      (console/print-current-board current-state (:moves-made current-state))

      ;is the game over? Loss or Draw Condition since win is impossible
      (if-let [{:keys [state next-state]} (game-over-state current-state)]
        (let [[new-stats play-again?] (console/handle-game-over-state next-state)]
          (if play-again?
            (do
              (reset! game-state (assoc board-state
                                        :stats new-stats
                                        :player-name player-name))
              (recur))
            new-stats))

        ;Continue game, player and computer input.
        (let [input (when (= (:current-player current-state) "X")
                      (handle-player-turn))
              {:keys [state next-state]} (compute-next-state current-state input)]
          (reset! game-state next-state)
          (case state
            :invalid-move ;as opposed to invalid 'input' the form of input would is  
            (do           ;fine, but in this condition the board spot is taken.
              (println "Invalid move! Spot already taken.")
              (recur))

            :continue
            (recur)))))))

(defn -main
  []
  (console/print-heading)
  (let [player-name (read-line)]
    (println (str "Okay " player-name ", welcome to the game!"))
    (let [final-stats (play-game player-name)]
      (console/print-stats final-stats 1))))
