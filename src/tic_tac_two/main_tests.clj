(ns tic-tac-two.main-tests
  (:require [clojure.test :refer :all]
            [tic-tac-two.main :as main]
            [tic-tac-two.console :as console]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Basic Testing             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest game-mechanics-test
  (testing "Basic board operations"
    (let [empty-board [[" " " " " "]
                       [" " " " " "]
                       [" " " " " "]]]

      (testing "Legal moves"
        (is (true? (main/legal-move? 0 0 empty-board)))
        (is (false? (main/legal-move? 0 0 (main/make-move empty-board 0 0 "X")))))

      (testing "Making moves"
        (is (= (get-in (main/make-move empty-board 0 0 "X") [0 0]) "X"))
        (is (= (main/make-move (main/make-move empty-board 0 0 "X") 0 0 "O")
               (main/make-move empty-board 0 0 "X"))))

      (testing "Available moves"
        (is (= 9 (count (main/get-available-moves empty-board))))
        (is (= 8 (count (main/get-available-moves (main/make-move empty-board 0 0 "X")))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              Mini-Max               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(main/minimax [["X" "X" nil]
               [nil "O" nil]
               [nil nil nil]]
              "O"
              4)

(deftest minimax-test
  (testing "When X can win in one move."
    (let [board [["X" "X" nil]
                 [nil "O" nil]
                 [nil nil nil]]
          result (main/minimax board "O" 4)]
      (is (= [0 [0 2]] result))))

  (testing "When O can win in one move."
    (let [board [["X" "X" "O"]
                 [nil "O" "X"]
                 [nil nil nil]]
          result (main/minimax board "O" 6)]
      (is (= [1 [2 0]] result))))

  (testing "Game is already won by X"
    (let [board [["X" "X" "X"]
                 ["O" "O" nil]
                 [nil nil nil]]
          result (main/minimax board "O" 5)]
      (is (= [-1 nil] result))))

  (testing "Game is in progress with no immediate win"
    (let [board [["X" nil nil]
                 [nil "O" nil]
                 [nil nil nil]]
          result (main/minimax board "X" 3)]
      (is (= [0 [2 2]] result))))
  
  (testing "Example where 'X' is about to win"
    (let [board [["X" "O" "X"]
                 [nil "X" nil]
                 ["O" "O" "X"]]
          result (main/minimax board "X" 7)]
      (is (= [1 nil] result))))
  
  (testing "Example where 'O' is about to win"
    (let [board  [["X" nil nil]
                  ["X" "O" "O"]
                  ["O" "X" nil]]
          result (main/minimax board "O" 5)]
      (is (= [1 [0 2]] result))))
  
  (testing "A complete draw situation"
    (let [board  [["X" "O" "X"]
                  ["O" "X" "O"]
                  ["O" "X" "O"]]
          result (main/minimax board "X" 10)]
      (is (= [0 nil] result))))
  
  (testing "X will win on the next turn"
    (let [board  [["X" "O" "X"]
                  ["O" "X" "O"]
                  ["O" "X" nil]]
          result (main/minimax board "X" 9)]
      (is (= [1 [2 2]] result))))
  
  (testing "'O' has won"
    (let [board  [["X" "O" nil]
                  ["X" "O" nil]
                  [nil "O" nil]]
          result (main/minimax board "X" 3)]
      (is (= [-1 nil] result))))
  
  (testing "Minimax with invalid moves"
    (let [board [["X" "O" "X"]
                 ["O" "X" "O"]
                 ["O" "X" "O"]]
          result (main/minimax board "X" 3)]
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
        (let [[score move] (main/minimax almost-win-board "O" 3)]
          (is (= move [1 2]))))

      (testing "Computer blocks player's winning move"
        (let [[score move] (main/minimax block-win-board "O" 3)]
          (is (= move [0 2]))))))

  (testing "Minimax endgame detection"
    (let [won-board [["X" "X" "X"]
                     ["O" "O" " "]
                     [" " " " " "]]
          draw-board [["X" "O" "X"]
                      ["O" "X" "O"]
                      ["O" "X" "O"]]]

      (testing "Detects won game"
        (let [[score move] (main/minimax won-board "O" 3)]
          (is (= score -1))
          (is (nil? move))))

      (testing "Detects drawn game"
        (let [[score move] (main/minimax draw-board "X" 3)]
          (is (= score 0)))))))
          


(run-tests)