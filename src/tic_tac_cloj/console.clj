(ns tic-tac-cloj.console
  (:require [clojure.string :refer [blank?]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;             console input           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn obtain-player-input []
  (let [row (get-valid-input "\nEnter your row [0, 1, 2]:" valid-input?)
        col (get-valid-input "Enter your column [0, 1, 2]:" valid-input?)]
    [row col]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            console output           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def title
  (str "======================\n"
       "Lor's Tic-Tac-Toe Game\n"
       "======================\n"))

(defn print-heading
  []
  (println title)
  (println " Enter your REAL name"))

(defn welcome!
  []
  (print-heading)
  (let [player-name (read-line)]
    (println (str "Okay " player-name ", welcome to the game!"))
    player-name))

(defn print-board-and-moves
  [board moves-made?]
  (println (str "\nMoves: " moves-made?))
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

(defn end-of-game-message
  [is-final?]
  (if is-final?
    (println "\nThanks for playing!\n")
    (println "\nCurrent Stats: ")))

(defn print-end-of-game-stats
  [stats]
  (end-of-game-message false)
  (wld-factory discrete-value-format stats)
  (wld-factory stat-format stats))  

(defn print-draw-or-loss
  "Since the computer can't win, it is technically only necessary to to write the computer perspective."
  [loss-or-draw is-player-win? player-name]
  (if loss-or-draw
    (println "\n*~Game ends in a draw!~*\n")
    (println (str "Player " (if is-player-win? player-name "Computer") " wins!"))))

(defn play-again?
  []
  (println "\nWould you like to play again? (yes/no)")
  (let [response (clojure.string/lower-case (read-line))]
    (contains? #{"y" "yes"} response)))

(defn handle-game-over-state
  [state]
  (let [{:keys [stats winner player-name]} state
        is-player-win? (= winner "X")]
    (print-draw-or-loss (nil? winner) is-player-win? player-name)
    (print-end-of-game-stats stats)
    (let [play-again (play-again?)]
      [stats play-again])))