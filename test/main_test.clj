(ns main-test
  (:require [clojure.test :refer :all]
            [tic-tac-cloj.main :as main]
            [tic-tac-cloj.console :as console]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                               ;              process-turn testing            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest process-turn-test

  (testing "Player makes valid move to empty space"
    (let [initial-state {:board [["" "" ""]
                                 ["" "" ""]
                                 ["" "" ""]]
                         :current-player "X"
                         :moves-made 0}
          result (main/process-turn initial-state [0 0])]
      (is (= :continue (:status result)))
      (is (= "X" (get-in (:state result) [:board 0 0])))))

  (testing "Player attempts move to occupied space"
    (let [initial-state {:board [["X" "" ""]
                                 ["" "" ""]
                                 ["" "" ""]]
                         :current-player "X"
                         :moves-made 1}
          result (main/process-turn initial-state [0 0])]
      (is (= :invalid (:status result)))
      (is (= initial-state (:state result)))))

  (testing "Game continues after valid moves"
    (let [initial-state {:board [["X" "O" ""]
                                 ["" "" ""]
                                 ["" "" ""]]
                         :current-player "X"
                         :moves-made 2}
          result (main/process-turn initial-state [1 1])]
      (is (= :continue (:status result)))
      (is (= 3 (:moves-made (:state result))))))

  (testing "Moves-made counter increments after valid move"
    (let [initial-state {:board [["" "" ""]
                                 ["" "" ""]
                                 ["" "" ""]]
                         :current-player "X"
                         :moves-made 0}
          result (main/process-turn initial-state [0 0])]
      (is (= 1 (:moves-made (:state result))))))

  (testing "Current player alternates after valid move"
    (let [initial-state {:board [["" "" ""]
                                 ["" "" ""]
                                 ["" "" ""]]
                         :current-player "X"
                         :moves-made 0}
          result (main/process-turn initial-state [0 0])]
      (is (= "O" (:current-player (:state result))))))

  (testing "Board state remains unchanged after invalid move"
    (let [initial-board [["X" "O" ""]
                         ["" "X" ""]
                         ["" "" "O"]]
          initial-state {:board initial-board
                         :current-player "X"
                         :moves-made 5}
          result (main/process-turn initial-state [0 0])]
      (is (= initial-board (:board (:state result))))))

  (testing "Valid move on nearly full board"
    (let [initial-state {:board [["X" "O" "X"]
                                 ["O" "X" "O"]
                                 ["O" "X" ""]]
                         :current-player "X"
                         :moves-made 8}
          result (main/process-turn initial-state [2 2])]
      (is (= :continue (:status result)))
      (is (= "X" (get-in (:state result) [:board 2 2]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;    handle-game-completion testing   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest handle-game-completion-test
  (testing "Game should continue when 8 moves made but no winner"
    (let [current-state {:board [["X" "O" "X"]
                                ["O" "X" "O"]
                                ["O" "X" ""]]
                        :moves-made 8}
          result (main/handle-game-completion current-state)]
      (is (= :continue (:status result)))))

  (testing "Stats should accumulate correctly across multiple games"
    (with-redefs [console/handle-game-over-state (fn [_] [{:wins 5 :losses 3 :draws 2} false])]
      (let [current-state {:board [["X" "X" "X"]
                                  ["O" "O" ""]
                                  ["" "" ""]]
                          :moves-made 5
                          :stats {:wins 4 :losses 3 :draws 2}}
            result (main/handle-game-completion current-state)]
        (is (= {:wins 5 :losses 3 :draws 2} (:stats result)))))))


(run-tests)