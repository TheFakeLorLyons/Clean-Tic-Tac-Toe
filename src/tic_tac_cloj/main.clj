;A straightforward and "clean" implementation of Tic-Tac-Toe
;Lorelai Lyons

(ns tic-tac-cloj.main
  (:require 
            [tic-tac-cloj.console :as console]
            [tic-tac-cloj.state :as state]
            [tic-tac-cloj.ai :as ai]))

(defn play-game 
  "This is the main game loop. Since the program is optimized/unlosable for the
    computer, technically only a draw or a loss may occur. Accompanied are tests,
    and any further testing is welcome and I can provide further testing upon request."
  [player-name]
  (println (str "Okay " player-name ", welcome to the game!"))
  (reset! state/game-state (assoc state/board-state :player-name player-name))
  (loop []
    (let [current-state @state/game-state]
      (console/print-current-board current-state (:moves-made current-state))

      ;is the game over? Loss or Draw Condition since win is impossible
      (if-let [{:keys [state next-state]} (state/check-game-over current-state)]
        (let [[new-stats play-again?] (console/handle-game-over-state next-state)]
          (if play-again?
            (do
              (reset! state/game-state (assoc state/board-state
                                        :stats new-stats
                                        :player-name player-name))
              (recur))
            new-stats))

        ;Continue game, player and computer input.
        (let [input (when (= (:current-player current-state) "X")
                      (console/handle-player-turn))
              {:keys [state next-state]} (ai/compute-next-game-state current-state input)]
          (reset! state/game-state next-state)
          (case state
            :invalid-move ;as opposed to invalid 'input' the form of input would is  
            (do           ;fine, but in this condition the board spot is taken.
              (println "Invalid move! Spot already taken.")
              (recur))

            :continue
            (recur)))))))

(defn -main
  "The entry point to the program. It first prints a heading and then takes
    user input simply for their name. After welcoming the player, it will
    invoke the main game loop. The game will exit and print a thank you
    message when the player has chosen not to stop playing."
  []
  (console/print-heading)
  (let [player-name (read-line)]
    (play-game player-name)
    (console/end-of-game-message true)))
