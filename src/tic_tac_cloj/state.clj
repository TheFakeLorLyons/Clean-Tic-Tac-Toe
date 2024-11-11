(ns tic-tac-cloj.state)

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

(defn check-game-over [current-state]
  "This function takes the current state of the game and evaluates if there is a winner
   first by calling the check-winner function, and subsequently in the event that there
   have been 9 moves completed."
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
                       (update :stats #(update % :draws inc)))})))

(defn update-board-state
  "Update-board-state functions by taking appropriate symbol ('X' or 'O') into the 
    appropriate spot in the state vectors for both the player and computer."
  [board row col player]
  (if (legal-move? row col board)
    (assoc-in board [row col] player)
    board))

(defn evaluate-and-update-game
  "Responsible for taking the user input and validating it agains the model for input 
      provided via valid-input?. Calls make-move, which is shared between the human and AI.
Responsible for the computer's turn. Calls minimax to determine the most appropriate move 
      and make move to update the board state."
  [current-state [row col]]
  (let [{:keys [board current-player]} current-state
        new-board (update-board-state board row col current-player)]
    (if (= new-board board)
      {:state :invalid-move
       :next-state current-state}
      {:state :continue
       :next-state (-> current-state
                       (assoc :board new-board)
                       (update :moves-made inc)
                       (assoc :current-player (if (= current-player "X") "O" "X")))})))