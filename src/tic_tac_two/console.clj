(ns tic-tac-two.console)

(def title
  (str "======================\n"
       "Lor's Tic-Tac-Toe Game\n"
       "======================\n"))

(defn print-heading []
  (println title)
  (println "  Enter your REAL name"))

(defn print-board [board]
  (doseq [row board]
    (println (clojure.string/join " | " (map #(or % " ") row)))))


(defn print-stats [stats is-final?]
  (if is-final?
    (do
      (println "\nThanks for playing!")
      (println "Final Stats:"))
    (println "\nCurrent Stats:"))
  (println (str "Wins: " (:wins stats)))
  (println (str "Losses: " (:losses stats)))
  (println (str "Draws: " (:draws stats)))
  (println (str "Win Rate: "
                (if (zero? (+ (:wins stats) (:losses stats) (:draws stats)))
                  "0.0"
                  (format "%.1f" (* 100.0 (/ (float (:wins stats))
                                             (+ (float (:wins stats))
                                                (float (:losses stats))
                                                (float (:draws stats))))))))
           "%\n"))

(defn print-draw-or-computer-win
  "Since the computer can't win we really only need to write the computer part."
  [loss-or-draw is-player-win? player-name]
  (if loss-or-draw
    (println "Game ends in a draw!")
    (println (str "Player " (if is-player-win? player-name "Computer") " wins!"))))


(defn play-again? []
  (println "\nWould you like to play again? (yes/no)")
  (let [response (read-line)]
    (contains? #{"y" "yes"} response)))

(defn update-stats [stats result]
  (case result
    :win (update stats :wins inc)
    :loss (update stats :losses inc)
    :draw (update stats :draws inc)))