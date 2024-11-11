;A straightforward and "clean" implementation of Tic-Tac-Toe
;Lorelai Lyons

(ns tic-tac-cloj.main
  (:require [tic-tac-cloj.ai :as ai]
            [tic-tac-cloj.console :as console]
            [tic-tac-cloj.state :as state]))

(defn play-game
  "This is the main game loop that coordinates state transitions and UI. Since the program 
    is optimized/unlosable for the computer, technically only a draw or a loss may occur, 
    but the game is written conducive to the possibility that we may want to incorporate 
    'difficulty' in the form of chance (introduced mistakes into the computer's play). 
    Accompanied are tests to prove the validity of each function, and any further testing 
    is welcome and I can provide further testing upon request. I tried to include a fair 
    number of individual tests as well as some integration testing on some of the more 
    important aspect of the game."
  [player-name]
  (loop [current-state (assoc state/board-state :player-name player-name)] 
    (let [_ (console/print-current-board current-state (:moves-made current-state))
          completion-result (state/handle-game-completion current-state)]
      (case (:status completion-result) 
        :quit (:stats completion-result)
        :restart (recur (assoc state/board-state
                               :stats (:stats completion-result)
                               :player-name player-name)) 
        :continue (let [move (when (= (:current-player current-state) "X")
                               (console/obtain-player-input))
                        turn-result (ai/process-turn current-state move)]
                    (case (:status turn-result)
                      :invalid (do
                                 (println "Invalid move! Spot already taken.")
                                 (recur current-state)) 
                      :continue (recur (:state turn-result))))))))

(defn -main
  "The entry point to the program. (welcome!) first prints a heading and then takes
    user input simply for their name. After (welcome!)ing the player, it will invoke 
    the (play-game) loop. The game will exit and print an end-of-game thank you 
    message when the player has chosen not to stop playing."
  []
  (let [player-name (console/welcome!)]
    (play-game player-name)
    (console/end-of-game-message true)))
