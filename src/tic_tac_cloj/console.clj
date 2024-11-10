(ns tic-tac-cloj.console)

(def title
  (str "======================\n"
       "Lor's Tic-Tac-Toe Game\n"
       "======================\n"))

(defn print-heading
  []
  (println title)
  (println " Enter your REAL name"))

(defn print-board-and-moves
  [board moves-made?]
  (println (str "Moves: " moves-made?))
  (doseq [row board]
    (println (clojure.string/join " | " (map #(or % " ") row)))))

(defn print-current-board
  [state moves-made?]
  (print-board-and-moves (:board state) moves-made?))

(defn calculate-wld-rate
  [stats which?]
  (let [wins (:wins stats)
        losses (:losses stats)
        draws (:draws stats)
        selected (which? stats)]
    (if (zero? (+ wins losses draws))
      "0.0"
      (format "%.1f" (* 100.0 (/ (float selected)
                                 (+ wins losses draws)))))))

(defn win-loss-draw-label
  [stat?]
  (let [ret (cond (= stat? :wins) "Win"
                  (= stat? :draws) "Draw"
                  (= stat? :losses) "Loss")]
    ret))

(defn stat-format
  [stats which?]
  (let [label (win-loss-draw-label which?)]
    (println (str label " Rate: " (calculate-wld-rate stats which?) "%"))))

(defn discrete-value-format
  [stats which?]
  (let [label (win-loss-draw-label which?)
        value (get stats which? 0)]
    (println (str label ": " value))))

(defn wld-factory
  "Takes a function and stats, applies the function to each stat type"
  [f stats]
  (doseq [stat [:wins :losses :draws]]
    (f stats stat)))

(defn print-end-of-game-stats
  [stats]
  (wld-factory discrete-value-format stats)
  (wld-factory stat-format stats))  

(defn print-stats
  [stats is-final?]
  (when is-final?
    (println "\nThanks for playing!\n"))
  (print-end-of-game-stats stats))

(defn print-draw-or-computer-win
  "Since the computer can't win, it is technically only necessary to to write the computer perspective."
  [loss-or-draw is-player-win? player-name]
  (if loss-or-draw
    (println "Game ends in a draw!")
    (println (str "Player " (if is-player-win? player-name "Computer") " wins!"))))

(defn play-again?
  []
  (println "\nWould you like to play again? (yes/no)")
  (let [response (read-line)]
    (contains? #{"y" "yes"} response)))

(defn update-stats [stats result]
  (update stats result inc))

(defn handle-game-over-state
  [state]
  (let [{:keys [board stats winner player-name]} state
        is-player-win? (= winner "X")]
    (print-draw-or-computer-win (nil? winner) is-player-win? player-name)
    (print-stats stats 0)  ; Print current game stats
    (let [play-again (play-again?)]
      (if play-again
        [stats true]
        (do
          (print-stats stats 1)  ; Only print final stats with goodbye if player is done
          [stats false])))))