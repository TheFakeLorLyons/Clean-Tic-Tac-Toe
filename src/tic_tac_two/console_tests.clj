(ns tic-tac-two.console-tests
  (:require [clojure.test :refer :all] 
            [tic-tac-two.console :as console]))

(deftest stats-tracking-test
  (testing "Stats update correctly"
    (let [initial-stats {:wins 0 :losses 0 :draws 0}]

      (testing "Win increment"
        (is (= {:wins 1 :losses 0 :draws 0}
               (console/update-stats initial-stats :win))))

      (testing "Loss increment"
        (is (= {:wins 0 :losses 1 :draws 0}
               (console/update-stats initial-stats :loss))))

      (testing "Draw increment"
        (is (= {:wins 0 :losses 0 :draws 1}
               (console/update-stats initial-stats :draw))))

      (testing "Multiple updates"
        (is (= {:wins 1 :losses 2 :draws 1}
               (-> initial-stats
                   (console/update-stats :win)
                   (console/update-stats :loss)
                   (console/update-stats :loss)
                   (console/update-stats :draw))))))))




(deftest stats-update-test
  (testing "Stats update correctly"
    (let [initial-stats {:wins 0 :losses 0 :draws 0}]
      (is (= {:wins 1 :losses 0 :draws 0}
             (console/update-stats initial-stats :win)))
      (is (= {:wins 0 :losses 1 :draws 0}
             (console/update-stats initial-stats :loss)))
      (is (= {:wins 0 :losses 0 :draws 1}
             (console/update-stats initial-stats :draw))))))

(deftest calculate-wld-rate-test
  (testing "WLD rate calculations"
    (let [stats {:wins 1 :losses 2 :draws 1}]
      (is (= "25.0" (console/calculate-wld-rate stats :wins)))
      (is (= "50.0" (console/calculate-wld-rate stats :losses)))
      (is (= "25.0" (console/calculate-wld-rate stats :draws))))

    (testing "Zero case"
      (let [empty-stats {:wins 0 :losses 0 :draws 0}]
        (is (= "0.0" (console/calculate-wld-rate empty-stats :wins)))))))

(deftest win-loss-draw-label-test
  (testing "Label generation"
    (is (= "Win" (console/win-loss-draw-label :wins)))
    (is (= "Loss" (console/win-loss-draw-label :losses)))
    (is (= "Draw" (console/win-loss-draw-label :draws)))))




(run-tests)
