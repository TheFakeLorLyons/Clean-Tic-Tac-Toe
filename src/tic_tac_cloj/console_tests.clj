(ns tic-tac-cloj.console-tests
  (:require [clojure.test :refer :all] 
            [tic-tac-cloj.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;       stats tracking Testing        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                    ;      calculate-wld-rate-test Testing    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest calculate-wld-rate-test
  (testing "WLD rate calculations"
    (let [stats {:wins 1 :losses 2 :draws 1}]
      (is (= "25.0" (console/calculate-wld-rate stats :wins)))
      (is (= "50.0" (console/calculate-wld-rate stats :losses)))
      (is (= "25.0" (console/calculate-wld-rate stats :draws))))

    (testing "Zero case"
      (let [empty-stats {:wins 0 :losses 0 :draws 0}]
        (is (= "0.0" (console/calculate-wld-rate empty-stats :wins)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;         wld-label testing           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest win-loss-draw-label-test
  (testing "Label generation"
    (is (= "Win" (console/win-loss-draw-label :wins)))
    (is (= "Loss" (console/win-loss-draw-label :losses)))
    (is (= "Draw" (console/win-loss-draw-label :draws)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;       update-stats Testing          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest update-stats-tests
  (testing "Updating initial empty stats"
    (let [initial-stats {:wins 0 :losses 0 :draws 0}]
      (is (= {:wins 1 :losses 0 :draws 0}
             (console/update-stats initial-stats :win))
          "Should increment wins from zero")
      (is (= {:wins 0 :losses 1 :draws 0}
             (console/update-stats initial-stats :loss))
          "Should increment losses from zero")
      (is (= {:wins 0 :losses 0 :draws 1}
             (console/update-stats initial-stats :draw))
          "Should increment draws from zero")))

  (testing "Updating non-zero stats"
    (let [existing-stats {:wins 5 :losses 3 :draws 2}]
      (is (= {:wins 6 :losses 3 :draws 2}
             (console/update-stats existing-stats :win))
          "Should increment existing win count")
      (is (= {:wins 5 :losses 4 :draws 2}
             (console/update-stats existing-stats :loss))
          "Should increment existing loss count")
      (is (= {:wins 5 :losses 3 :draws 3}
             (console/update-stats existing-stats :draw))
          "Should increment existing draw count")))

  (testing "Multiple sequential updates"
    (let [initial-stats {:wins 0 :losses 0 :draws 0}
          after-win (console/update-stats initial-stats :win)
          after-two-wins (console/update-stats after-win :win)
          final-stats (console/update-stats after-two-wins :win)]
      (is (= {:wins 3 :losses 0 :draws 0} final-stats)
          "Should correctly track multiple wins")))

  (testing "Mixed sequence of updates"
    (let [initial-stats {:wins 0 :losses 0 :draws 0}
          after-win (console/update-stats initial-stats :win)
          after-loss (console/update-stats after-win :loss)
          final-stats (console/update-stats after-loss :draw)]
      (is (= {:wins 1 :losses 1 :draws 1} final-stats)
          "Should correctly track mixed game results")))

  (testing "High volume of updates"
    (let [initial-stats {:wins 99 :losses 99 :draws 99}]
      (is (= {:wins 100 :losses 99 :draws 99}
             (console/update-stats initial-stats :win))
          "Should handle high numbers correctly")))

  (testing "Updates with large existing counts"
    (let [large-stats {:wins 9999 :losses 9999 :draws 9999}]
      (is (= {:wins 10000 :losses 9999 :draws 9999}
             (console/update-stats large-stats :win))
          "Should handle very large numbers")))

  (testing "Updates with uneven initial stats"
    (let [uneven-stats {:wins 10 :losses 2 :draws 5}]
      (is (= {:wins 10 :losses 3 :draws 5}
             (console/update-stats uneven-stats :loss))
          "Should correctly update uneven stats")))

  (testing "Multiple draw updates"
    (let [initial-stats {:wins 0 :losses 0 :draws 0}
          after-first-draw (console/update-stats initial-stats :draw)
          after-second-draw (console/update-stats after-first-draw :draw)]
      (is (= {:wins 0 :losses 0 :draws 2} after-second-draw)
          "Should correctly track multiple draws")))

  (testing "Stats preservation"
    (let [original-stats {:wins 5 :losses 3 :draws 2}
          updated-stats (console/update-stats original-stats :win)]
      (is (= original-stats {:wins 5 :losses 3 :draws 2})
          "Should not modify original stats object")
      (is (not= original-stats updated-stats)
          "Should return new stats object")))

  (testing "Zero-sum verification"
    (let [initial-stats {:wins 1 :losses 1 :draws 1}
          wins (get (console/update-stats initial-stats :win) :wins)
          losses (get (console/update-stats initial-stats :loss) :losses)
          draws (get (console/update-stats initial-stats :draw) :draws)]
      (is (= [2 2 2] [wins losses draws])
          "All types of updates should increment by same amount"))))

(run-tests)
