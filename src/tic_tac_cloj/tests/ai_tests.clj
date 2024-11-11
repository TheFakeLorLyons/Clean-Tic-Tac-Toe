(ns tic-tac-cloj.tests.ai-tests
  (:require [clojure.test :refer :all]
            [tic-tac-cloj.ai :as ai]))

;Mock functions for testing
(def empty-board
  [[nil nil nil]
   [nil nil nil]
   [nil nil nil]])

(def initial-state
  {:board empty-board
   :current-player "X"
   :game-status :in-progress})

(defn mock-minimax [board player depth]
  [0 [1 1]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;         minimax testing             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ai/minimax [["X" "X" nil]
             [nil "O" nil]
             [nil nil nil]]
            "O"
            4)

(deftest minimax-test
  (testing "When X can win in one move."
    (let [board [["X" "X" nil]
                 [nil "O" nil]
                 [nil nil nil]]
          result (ai/minimax board "O" 4)]
      (is (= [0 [0 2]] result))))

  (testing "When O can win in one move."
    (let [board [["X" "X" "O"]
                 [nil "O" "X"]
                 [nil nil nil]]
          result (ai/minimax board "O" 6)]
      (is (= [1 [2 0]] result))))

  (testing "Game is already won by X"
    (let [board [["X" "X" "X"]
                 ["O" "O" nil]
                 [nil nil nil]]
          result (ai/minimax board "O" 5)]
      (is (= [-1 nil] result))))

  (testing "Game is in progress with no immediate win"
    (let [board [["X" nil nil]
                 [nil "O" nil]
                 [nil nil nil]]
          result (ai/minimax board "X" 3)]
      (is (= [0 [2 2]] result))))

  (testing "Example where 'X' is about to win"
    (let [board [["X" "O" "X"]
                 [nil "X" nil]
                 ["O" "O" "X"]]
          result (ai/minimax board "X" 7)]
      (is (= [1 nil] result))))

  (testing "Example where 'O' is about to win"
    (let [board  [["X" nil nil]
                  ["X" "O" "O"]
                  ["O" "X" nil]]
          result (ai/minimax board "O" 5)]
      (is (= [1 [0 2]] result))))

  (testing "A complete draw situation"
    (let [board  [["X" "O" "X"]
                  ["O" "X" "O"]
                  ["O" "X" "O"]]
          result (ai/minimax board "X" 10)]
      (is (= [0 nil] result))))

  (testing "X will win on the next turn"
    (let [board  [["X" "O" "X"]
                  ["O" "X" "O"]
                  ["O" "X" nil]]
          result (ai/minimax board "X" 9)]
      (is (= [1 [2 2]] result))))

  (testing "'O' has won"
    (let [board  [["X" "O" nil]
                  ["X" "O" nil]
                  [nil "O" nil]]
          result (ai/minimax board "X" 3)]
      (is (= [-1 nil] result))))

  (testing "Minimax with invalid moves"
    (let [board [["X" "O" "X"]
                 ["O" "X" "O"]
                 ["O" "X" "O"]]
          result (ai/minimax board "X" 3)]
      (is (= [0 nil] result)))))


(deftest minimax-algorithm-test
  (testing "Minimax immediate wins and blocks"
    (let [almost-win-board [["X" "X" " "]
                            ["O" "O" " "]
                            [" " " " " "]]
          block-win-board [["X" "X" " "]
                           [" " "O" " "]
                           [" " " " " "]]]

      (testing "Computer finds winning move"
        (let [[score move] (ai/minimax almost-win-board "O" 3)]
          (is (= move [1 2]))))

      (testing "Computer blocks player's winning move"
        (let [[score move] (ai/minimax block-win-board "O" 3)]
          (is (= move [0 2]))))))

  (testing "Minimax endgame detection"
    (let [won-board [["X" "X" "X"]
                     ["O" "O" " "]
                     [" " " " " "]]
          draw-board [["X" "O" "X"]
                      ["O" "X" "O"]
                      ["O" "X" "O"]]]

      (testing "Detects won game"
        (let [[score move] (ai/minimax won-board "O" 3)]
          (is (= score -1))
          (is (nil? move))))

      (testing "Detects drawn game"
        (let [[score move] (ai/minimax draw-board "X" 3)]
          (is (= score 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;    get-available-moves Testing      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest get-available-moves-tests
  (testing "Empty board"
    (is (= #{[0 0] [0 1] [0 2]
             [1 0] [1 1] [1 2]
             [2 0] [2 1] [2 2]}
           (set (ai/get-available-moves [[" " " " " "]
                                         [" " " " " "]
                                         [" " " " " "]])))))

  (testing "Partially filled board"
    (is (= #{[0 1] [0 2]
             [1 0] [1 2]
             [2 0] [2 1] [2 2]}
           (set (ai/get-available-moves [["X" " " " "]
                                         [" " "O" " "]
                                         [" " " " " "]])))))

  (testing "Nearly full board"
    (is (= #{[2 2]}
           (set (ai/get-available-moves [["X" "O" "X"]
                                         ["O" "X" "O"]
                                         ["X" "O" " "]])))))

  (testing "Full board"
    (is (empty? (ai/get-available-moves [["X" "O" "X"]
                                         ["O" "X" "O"]
                                         ["X" "O" "X"]]))))

  (testing "Board with multiple empty spaces"
    (is (= #{[0 0] [1 1] [2 2]}
           (set (ai/get-available-moves [[" " "X" "O"]
                                         ["X" " " "O"]
                                         ["X" "O" " "]]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;      evaluate-board Testing         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest evaluate-board-tests
  (testing "Winning positions for X"
    (is (= 1 (ai/evaluate-position [["X" "X" "X"]
                                    ["O" "O" " "]
                                    [" " " " " "]] "X")))
    (is (= 1 (ai/evaluate-position [["X" "O" " "]
                                    ["X" "O" " "]
                                    ["X" " " " "]] "X")))
    (is (= 1 (ai/evaluate-position [["X" "O" " "]
                                    ["O" "X" " "]
                                    [" " " " "X"]] "X"))))

  (testing "Winning positions for O"
    (is (= -1 (ai/evaluate-position [["X" "O" "X"]
                                     ["X" "O" " "]
                                     [" " "O" " "]] "X")))
    (is (= -1 (ai/evaluate-position [["X" "O" " "]
                                     ["X" "O" " "]
                                     [" " "O" "X"]] "X")))
    (is (= 1 (ai/evaluate-position [["X" "O" " "]
                                    ["X" "O" " "]
                                    [" " "O" "X"]] "O"))))

  (testing "Draw positions"
    (is (= 0 (ai/evaluate-position [["X" "O" "X"]
                                    ["O" "X" "O"]
                                    ["O" "X" "O"]] "X")))
    (is (= 0 (ai/evaluate-position [["X" "O" "X"]
                                    ["O" "X" "O"]
                                    ["O" "X" "O"]] "O"))))

  (testing "Ongoing game positions"
    (is (= 0 (ai/evaluate-position [[" " " " " "]
                                    [" " "X" " "]
                                    [" " " " " "]] "X")))
    (is (= 0 (ai/evaluate-position [["X" "O" " "]
                                    [" " "X" " "]
                                    [" " " " "O"]] "O")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;     ai/compute-next-game-state testing      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest compute-next-state-tests
  (testing "Handles player X move"
    (let [state {:board [[" " " " " "]
                         [" " " " " "]
                         [" " " " " "]]
                 :moves-made 0
                 :current-player "X"}
          result (ai/compute-next-game-state state [0 0])]
      (is (= "X" (get-in result [:next-state :board 0 0])))))

  (testing "Computer makes winning move"
    (let [state {:board [["O" "O" " "]
                         [" " "X" " "]
                         ["X" " " " "]]
                 :moves-made 4
                 :current-player "O"}
          result (ai/compute-next-game-state state nil)]
      (is (= "O" (get-in result [:next-state :board 0 2])))))

  (testing "Computer blocks player win"
    (let [state {:board [["X" "X" " "]
                         [" " "O" " "]
                         [" " " " " "]]
                 :moves-made 3
                 :current-player "O"}
          result (ai/compute-next-game-state state nil)]
      (is (= "O" (get-in result [:next-state :board 0 2])))))

  (testing "Invalid player move handled"
    (let [state {:board [["X" " " " "]
                         [" " " " " "]
                         [" " " " " "]]
                 :moves-made 1
                 :current-player "X"}
          result (ai/compute-next-game-state state [0 0])]
      (is (= :invalid-move (:state result)))))

  (testing "Computer makes center move if available"
    (let [state {:board [["X" " " " "]
                         [" " " " " "]
                         [" " " " " "]]
                 :moves-made 1
                 :current-player "O"}
          result (ai/compute-next-game-state state nil)]
      (is (= "O" (get-in result [:next-state :board 1 1]))))))


(deftest compute-next-game-state-test
  ;Player X's valid move to empty position
  (testing "Player X makes a valid move"
    (let [input [0 0]
          result (ai/compute-next-game-state initial-state input)]
      (is (= "X" (get-in result [:board 0 0]))
          "Should place X at [0 0]")
      (is (= "O" (:current-player result))
          "Should switch to player O")))

  ;AI player O's move
  (testing "AI player O makes a move"
    (with-redefs [ai/minimax mock-minimax]
      (let [x-state {:board [[nil nil nil]
                             [nil nil nil]
                             [nil nil nil]]
                     :current-player "O"}
            result (ai/compute-next-game-state x-state nil)]
        (is (= "O" (get-in result [:board 1 1]))
            "AI should place O at center position [1 1]")
        (is (= "X" (:current-player result))
            "Should switch back to player X"))))

  ;Player X attempts invalid move
  (testing "Player X attempts move on occupied position"
    (let [board [[nil nil nil]
                 [nil "O" nil]
                 [nil nil nil]]
          state {:board board :current-player "X"}
          input [1 1]]
      (is (thrown? IllegalStateException
                   (ai/compute-next-game-state state input))
          "Should throw exception for invalid move")))

  ;Game winning move by X
  (testing "Player X makes winning move"
    (let [board [["X" "X" nil]
                 ["O" "O" nil]
                 [nil nil nil]]
          state {:board board :current-player "X"}
          input [0 2]
          result (ai/compute-next-game-state state input)]
      (is (= :winner-x (:game-status result))
          "Should recognize X as winner")))

  ;Game winning move by O
  (testing "AI player O makes winning move"
    (with-redefs [ai/minimax (fn [_ _ _] [10 [2 2]])]
      (let [board [["O" "O" nil]
                   ["X" "X" nil]
                   [nil nil nil]]
            input [0 2]
            state {:board board :current-player "O"}
            result (ai/compute-next-game-state state input)]
        (is (= :winner-o (:game-status result))
            "Should recognize O as winner"))))

  ;Draw game scenario
  (testing "Move leading to draw"
    (let [board [["X" "O" "X"]
                 ["O" "X" "O"]
                 ["O" "X" nil]]
          state {:board board :current-player "X"}
          input [2 2]
          result (ai/compute-next-game-state state input)]
      (is (= :draw (:game-status result))
          "Should recognize draw game")))

  ;First move of the game
  (testing "First move of the game"
    (let [result (ai/compute-next-game-state initial-state [0 0])]
      (is (= empty-board
             (assoc-in (:board initial-state) [0 0] "X"))
          "Should only modify the specified position")
      (is (= :in-progress (:game-status result))
          "Game should still be in progress")))

  ;: AI blocking opponent's winning move
  (testing "AI blocks opponent's winning move"
    (with-redefs [ai/minimax (fn [_ _ _] [0 [0 2]])]
      (let [board [["X" "X" nil]
                   ["O" nil nil]
                   [nil nil nil]]
            state {:board board :current-player "O"}
            result (ai/compute-next-game-state state nil)]
        (is (= "O" (get-in result [:board 0 2]))
            "AI should block X's winning move"))))

  ;Edge case - empty input for player X
  (testing "Empty input for player X"
    (is (thrown? IllegalArgumentException
                 (ai/compute-next-game-state initial-state nil))
        "Should throw exception for nil input from player X"))

  ;Edge case - out of bounds move
  (testing "Out of bounds move"
    (is (thrown? IndexOutOfBoundsException
                 (ai/compute-next-game-state initial-state [3 3]))
        "Should throw exception for out of bounds move")))


(run-tests)