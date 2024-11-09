(ns tic-tac-two.console)

(def title
  (str "======================\n"
       "Lor's Tic-Tac-Toe Game\n"
       "======================\n"))

(defn print-heading
  []
  (println title)
  (println "  Enter your REAL name"))

(defn print-board
  [board]
  (doseq [row board]
    (println (clojure.string/join " | " (map #(or % " ") row)))))

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
  (if is-final?
    (println "\nThanks for playing!\n");Final stats were printed prior, so this says bye!
    (println "\nCurrent Stats:"))      ;Otherwise, print the current stats!
  (print-end-of-game-stats stats))     ;Printing the W/L/D rates above.

(defn print-draw-or-computer-win
  "Since the computer can't win we really only need to write the computer part."
  [loss-or-draw is-player-win? player-name]
  (if loss-or-draw
    (println "Game ends in a draw!")
    (println (str "Player " (if is-player-win? player-name "Computer") " wins!"))))

(defn play-again?
  []
  (println "\nWould you like to play again? (yes/no)")
  (let [response (read-line)]
    (contains? #{"y" "yes"} response)))

(defn update-stats
  [stats result]
  (let [updated-stats
        (case result
          :win (update stats :wins inc)
          :loss (update stats :losses inc)
          :draw (update stats :draws inc))]
    updated-stats))