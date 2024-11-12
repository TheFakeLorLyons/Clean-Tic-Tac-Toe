(ns tic-tac-cloj.state
  (:require [tic-tac-cloj.console :as console]))

(def board-state
  {:board [[" " " " " "]
           [" " " " " "]
           [" " " " " "]]
   :moves-made 0
   :stats {:wins 0 :losses 0 :draws 0}
   :player-name nil
   :current-player "X"})

(defn detect-winning-row
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

(defn check-game-over 
  "This function takes the current state of the game and evaluates if there is a winner
   first by calling the check-winner function, and subsequently in the event that there
   have been 9 moves completed. Returns a map containing the appropriate {:state and 
   :next-state} back to the calling function."
  [current-state]
  (let [{:keys [board moves-made]} current-state
        winner (detect-winning-row board)]
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
    (assoc-in board [row col] player))

(defn evaluate-and-update-game
  "Assesses submitted moves from both the player and the computer to see if the submitted
    move is legal and moves the game forward with the updated board state, passing back
    either the :continue mapped to key and updated board into :next-state, otherwise if
    the entry was invalid, it returns back the same state, indicating that the spot was
    already taken."
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