(ns tic-tac-cloj.tests.state-tests
  (:require [clojure.test :refer :all]
            [tic-tac-cloj.state :as state]
            [tic-tac-cloj.ai :as ai]
            [tic-tac-cloj.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Basic Testing             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest game-mechanics-test
  (testing "Basic board operations"
    (let [empty-board [[" " " " " "]
                       [" " " " " "]
                       [" " " " " "]]]

      (testing "Legal moves"
        (is (true? (state/legal-move? 0 0 empty-board)))
        (is (false? (state/legal-move? 0 0 (state/update-board-state empty-board 0 0 "X")))))

      (testing "Available moves"
        (is (= 9 (count (ai/get-available-moves empty-board))))
        (is (= 8 (count (ai/get-available-moves (state/update-board-state empty-board 0 0 "X"))))))

      (testing "Making moves"
        (is (= (get-in (state/update-board-state empty-board 0 0 "X") [0 0]) "X"))
        (is (= (state/update-board-state (state/update-board-state empty-board 0 0 "X") 0 0 "O")
               (state/update-board-state empty-board 0 0 "X")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;        check-winner Testing         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest check-winner-tests
  (testing "Horizontal win detection"
    (is (= "X" (state/detect-winning-row [["X" "X" "X"]
                                          [" " "O" " "]
                                          ["O" " " " "]])))
    (is (= "O" (state/detect-winning-row [[" " "X" " "]
                                          ["O" "O" "O"]
                                          ["X" " " "X"]])))
    (is (= "X" (state/detect-winning-row [[" " "O" " "]
                                          ["O" " " " "]
                                          ["X" "X" "X"]]))))

  (testing "Vertical win detection"
    (is (= "X" (state/detect-winning-row [["X" "O" " "]
                                          ["X" "O" " "]
                                          ["X" " " " "]])))
    (is (= "O" (state/detect-winning-row [["X" "O" " "]
                                          [" " "O" "X"]
                                          ["X" "O" " "]]))))

  (testing "Diagonal win detection"
    (is (= "X" (state/detect-winning-row [["X" "O" " "]
                                          ["O" "X" " "]
                                          [" " " " "X"]])))
    (is (= "O" (state/detect-winning-row [["X" " " "O"]
                                          [" " "O" "X"]
                                          ["O" " " "X"]]))))

  (testing "No winner detection"
    (is (nil? (state/detect-winning-row [[" " " " " "]
                                         [" " " " " "]
                                         [" " " " " "]])))
    (is (nil? (state/detect-winning-row [["X" "O" "X"]
                                         ["O" "X" "O"]
                                         ["O" "X" "O"]]))))

  (testing "Draw game detection"
    (is (nil? (state/detect-winning-row [["X" "O" "X"]
                                         ["X" "O" "O"]
                                         ["O" "X" "X"]])))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;      check-game-over testing        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest check-game-over-tests
  (testing "Detects X winner"
    (let [state {:board [["X" "X" "X"]
                         [" " "O" " "]
                         ["O" " " " "]]
                 :moves-made 5
                 :stats {:wins 0 :losses 0 :draws 0}}]
      (is (= :game-over (:state (state/check-game-over state))))
      (is (= 1 (get-in (state/check-game-over state) [:next-state :stats :wins])))))

  (testing "Detects O winner"
    (let [state {:board [["X" "X" " "]
                         [" " "O" " "]
                         ["O" "O" "O"]]
                 :moves-made 7
                 :stats {:wins 0 :losses 0 :draws 0}}]
      (is (= :game-over (:state (state/check-game-over state))))
      (is (= 1 (get-in (state/check-game-over state) [:next-state :stats :losses])))))

  (testing "Detects draw"
    (let [state {:board [["X" "O" "X"]
                         ["X" "O" "O"]
                         ["O" "X" "X"]]
                 :moves-made 9
                 :stats {:wins 0 :losses 0 :draws 0}}]
      (is (= :game-over (:state (state/check-game-over state))))
      (is (= 1 (get-in (state/check-game-over state) [:next-state :stats :draws])))))

  (testing "Game continues when no winner and moves < 9"
    (let [state {:board [["X" "O" " "]
                         [" " "X" " "]
                         [" " " " "O"]]
                 :moves-made 4
                 :stats {:wins 0 :losses 0 :draws 0}}]
      (is (nil? (state/check-game-over state)))))

  (testing "Detects diagonal winner"
    (let [state {:board [["X" "O" " "]
                         ["O" "X" " "]
                         [" " "O" "X"]]
                 :moves-made 6
                 :stats {:wins 0 :losses 0 :draws 0}}]
      (is (= :game-over (:state (state/check-game-over state))))))

  (testing "Correctly updates existing stats"
    (let [state {:board [["X" "X" "X"]
                         ["O" "O" " "]
                         [" " " " " "]]
                 :moves-made 5
                 :stats {:wins 1 :losses 2 :draws 1}}]
      (is (= 2 (get-in (state/check-game-over state) [:next-state :stats :wins]))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;         make-moves Testing          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest make-move-tests
  (testing "Legal moves"
    (is (= [["X" " " " "]
            [" " " " " "]
            [" " " " " "]]
           (state/update-board-state [[" " " " " "]
                                      [" " " " " "]
                                      [" " " " " "]] 0 0 "X")))
    (is (= [[" " " " " "]
            [" " "O" " "]
            [" " " " " "]]
           (state/update-board-state [[" " " " " "]
                                      [" " " " " "]
                                      [" " " " " "]] 1 1 "O"))))

  (testing "Illegal moves - occupied space"
    (is (= [["X" " " " "]
            [" " " " " "]
            [" " " " " "]]
           (state/update-board-state [["X" " " " "]
                                      [" " " " " "]
                                      [" " " " " "]] 0 0 "O")))
    (is (= [["X" "O" " "]
            [" " " " " "]
            [" " " " " "]]
           (state/update-board-state [["X" "O" " "]
                                      [" " " " " "]
                                      [" " " " " "]] 0 1 "X"))))

  (testing "Multiple moves sequence"
    (let [board [[" " " " " "]
                 [" " " " " "]
                 [" " " " " "]]
          board1 (state/update-board-state board 0 0 "X")
          board2 (state/update-board-state board1 1 1 "O")
          board3 (state/update-board-state board2 2 2 "X")]
      (is (= [["X" " " " "]
              [" " "O" " "]
              [" " " " "X"]] board3))))

  (testing "Edge case - full board"
    (let [full-board [["X" "O" "X"]
                      ["O" "X" "O"]
                      ["O" "X" "O"]]]
      (is (= full-board (state/update-board-state full-board 0 0 "X")))))

  (testing "Move at board edges"
    (is (= [[" " " " "X"]
            [" " " " " "]
            [" " " " " "]]
           (state/update-board-state [[" " " " " "]
                                      [" " " " " "]
                                      [" " " " " "]] 0 2 "X")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;   evaluate-and-update-game testing  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest handle-move-tests
  (testing "Valid move updates board"
    (let [state {:board [[" " " " " "]
                         [" " " " " "]
                         [" " " " " "]]
                 :moves-made 0
                 :current-player "X"}
          result (state/evaluate-and-update-game state [0 0])]
      (is (= :continue (:state result)))
      (is (= "X" (get-in result [:next-state :board 0 0])))))

  (testing "Invalid move returns same board"
    (let [state {:board [["X" " " " "]
                         [" " " " " "]
                         [" " " " " "]]
                 :moves-made 1
                 :current-player "O"}
          result (state/evaluate-and-update-game state [0 0])]
      (is (= :invalid-move (:state result)))
      (is (= state (:next-state result)))))

  (testing "Move increments moves-made"
    (let [state {:board [[" " " " " "]
                         [" " " " " "]
                         [" " " " " "]]
                 :moves-made 0
                 :current-player "X"}
          result (state/evaluate-and-update-game state [1 1])]
      (is (= 1 (get-in result [:next-state :moves-made])))))

  (testing "Move switches current player"
    (let [state {:board [[" " " " " "]
                         [" " " " " "]
                         [" " " " " "]]
                 :moves-made 0
                 :current-player "X"}
          result (state/evaluate-and-update-game state [0 0])]
      (is (= "O" (get-in result [:next-state :current-player])))))

  (testing "Move preserves rest of state"
    (let [state {:board [[" " " " " "]
                         [" " " " " "]
                         [" " " " " "]]
                 :moves-made 0
                 :current-player "X"
                 :stats {:wins 1 :losses 1 :draws 0}}
          result (state/evaluate-and-update-game state [0 0])]
      (is (= (:stats state) (get-in result [:next-state :stats]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;    handle-game-completion testing   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest handle-game-completion-test
  (testing "Game should continue when 8 moves made but no winner"
    (let [current-state {:board [["X" "O" "X"]
                                ["O" "X" "O"]
                                ["O" "X" ""]]
                        :moves-made 8}
          result (state/handle-game-completion current-state)]
      (is (= :continue (:status result)))))

  (testing "Stats should accumulate correctly across multiple games"
    (with-redefs [console/handle-game-over-state (fn [_] [{:wins 5 :losses 3 :draws 2} false])]
      (let [current-state {:board [["X" "X" "X"]
                                  ["O" "O" ""]
                                  ["" "" ""]]
                          :moves-made 5
                          :stats {:wins 4 :losses 3 :draws 2}}
            result (state/handle-game-completion current-state)]
        (is (= {:wins 5 :losses 3 :draws 2} (:stats result)))))))

(run-tests)