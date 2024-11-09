(ns tic-tac-cloj.main-tests
  (:require [clojure.test :refer :all]
            [tic-tac-cloj.main :as main]))

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
                                        ;         minimax testing             ;
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
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;        check-winner Testing         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest check-winner-tests
  (testing "Horizontal win detection"
    (is (= "X" (main/check-winner [["X" "X" "X"]
                                   [" " "O" " "]
                                   ["O" " " " "]])))
    (is (= "O" (main/check-winner [[" " "X" " "]
                                   ["O" "O" "O"]
                                   ["X" " " "X"]])))
    (is (= "X" (main/check-winner [[" " "O" " "]
                                   ["O" " " " "]
                                   ["X" "X" "X"]]))))
  
  (testing "Vertical win detection"
    (is (= "X" (main/check-winner [["X" "O" " "]
                                   ["X" "O" " "]
                                   ["X" " " " "]])))
    (is (= "O" (main/check-winner [["X" "O" " "]
                                   [" " "O" "X"]
                                   ["X" "O" " "]]))))

  (testing "Diagonal win detection"
    (is (= "X" (main/check-winner [["X" "O" " "]
                                   ["O" "X" " "]
                                   [" " " " "X"]])))
    (is (= "O" (main/check-winner [["X" " " "O"]
                                   [" " "O" "X"]
                                   ["O" " " "X"]]))))
  
  (testing "No winner detection"
    (is (nil? (main/check-winner [[" " " " " "]
                                  [" " " " " "]
                                  [" " " " " "]])))
    (is (nil? (main/check-winner [["X" "O" "X"]
                                  ["O" "X" "O"]
                                  ["O" "X" "O"]]))))
  
  (testing "Draw game detection"
    (is (nil? (main/check-winner [["X" "O" "X"]
                                  ["X" "O" "O"]
                                  ["O" "X" "X"]])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;         make-moves Testing          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest make-move-tests
  (testing "Legal moves"
    (is (= [["X" " " " "]
            [" " " " " "]
            [" " " " " "]]
           (main/make-move [[" " " " " "]
                            [" " " " " "]
                            [" " " " " "]] 0 0 "X")))
    (is (= [[" " " " " "]
            [" " "O" " "]
            [" " " " " "]]
           (main/make-move [[" " " " " "]
                            [" " " " " "]
                            [" " " " " "]] 1 1 "O"))))
  
  (testing "Illegal moves - occupied space"
    (is (= [["X" " " " "]
            [" " " " " "]
            [" " " " " "]]
           (main/make-move [["X" " " " "]
                            [" " " " " "]
                            [" " " " " "]] 0 0 "O")))
    (is (= [["X" "O" " "]
            [" " " " " "]
            [" " " " " "]]
           (main/make-move [["X" "O" " "]
                            [" " " " " "]
                            [" " " " " "]] 0 1 "X"))))
  
  (testing "Multiple moves sequence"
    (let [board [[" " " " " "]
                 [" " " " " "]
                 [" " " " " "]]
          board1 (main/make-move board 0 0 "X")
          board2 (main/make-move board1 1 1 "O")
          board3 (main/make-move board2 2 2 "X")]
      (is (= [["X" " " " "]
              [" " "O" " "]
              [" " " " "X"]] board3))))
  
  (testing "Edge case - full board"
    (let [full-board [["X" "O" "X"]
                      ["O" "X" "O"]
                      ["O" "X" "O"]]]
      (is (= full-board (main/make-move full-board 0 0 "X")))))
  
  (testing "Move at board edges"
    (is (= [[" " " " "X"]
            [" " " " " "]
            [" " " " " "]]
           (main/make-move [[" " " " " "]
                            [" " " " " "]
                            [" " " " " "]] 0 2 "X")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;    get-available-moves Testing      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest get-available-moves-tests
  (testing "Empty board"
    (is (= #{[0 0] [0 1] [0 2]
             [1 0] [1 1] [1 2]
             [2 0] [2 1] [2 2]}
           (set (main/get-available-moves [[" " " " " "]
                                           [" " " " " "]
                                           [" " " " " "]])))))
  
  (testing "Partially filled board"
    (is (= #{[0 1] [0 2]
             [1 0] [1 2]
             [2 0] [2 1] [2 2]}
           (set (main/get-available-moves [["X" " " " "]
                                           [" " "O" " "]
                                           [" " " " " "]])))))
  
  (testing "Nearly full board"
    (is (= #{[2 2]}
           (set (main/get-available-moves [["X" "O" "X"]
                                           ["O" "X" "O"]
                                           ["X" "O" " "]])))))
  
  (testing "Full board"
    (is (empty? (main/get-available-moves [["X" "O" "X"]
                                           ["O" "X" "O"]
                                           ["X" "O" "X"]]))))
  
  (testing "Board with multiple empty spaces"
    (is (= #{[0 0] [1 1] [2 2]}
           (set (main/get-available-moves [[" " "X" "O"]
                                           ["X" " " "O"]
                                           ["X" "O" " "]]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;      evaluate-board Testing         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest evaluate-board-tests
  (testing "Winning positions for X"
    (is (= 1 (main/evaluate-board [["X" "X" "X"]
                                   ["O" "O" " "]
                                   [" " " " " "]] "X")))
    (is (= 1 (main/evaluate-board [["X" "O" " "]
                                   ["X" "O" " "]
                                   ["X" " " " "]] "X")))
    (is (= 1 (main/evaluate-board [["X" "O" " "]
                                   ["O" "X" " "]
                                   [" " " " "X"]] "X"))))
  
  (testing "Winning positions for O"
    (is (= -1 (main/evaluate-board [["X" "O" "X"]
                                    ["X" "O" " "]
                                    [" " "O" " "]] "X")))
    (is (= -1 (main/evaluate-board [["X" "O" " "]
                                    ["X" "O" " "]
                                    [" " "O" "X"]] "X")))
    (is (= 1 (main/evaluate-board [["X" "O" " "]
                                   ["X" "O" " "]
                                   [" " "O" "X"]] "O"))))
  
  (testing "Draw positions"
    (is (= 0 (main/evaluate-board [["X" "O" "X"]
                                   ["O" "X" "O"]
                                   ["O" "X" "O"]] "X")))
    (is (= 0 (main/evaluate-board [["X" "O" "X"]
                                   ["O" "X" "O"]
                                   ["O" "X" "O"]] "O"))))
  
  (testing "Ongoing game positions"
    (is (= 0 (main/evaluate-board [[" " " " " "]
                                   [" " "X" " "]
                                   [" " " " " "]] "X")))
    (is (= 0 (main/evaluate-board [["X" "O" " "]
                                   [" " "X" " "]
                                   [" " " " "O"]] "O")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;       valid-input? Testing          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest valid-input-tests
  (testing "Valid numeric inputs"
    (is (true? (main/valid-input? "0")) "Should accept '0'")
    (is (true? (main/valid-input? "1")) "Should accept '1'")
    (is (true? (main/valid-input? "2")) "Should accept '2'")))

  (testing "Non-numeric inputs"
    (is (false? (main/valid-input? "abc")) "Should reject alphabetic input")
    (is (false? (main/valid-input? "1.5")) "Should reject decimal numbers")
    (is (false? (main/valid-input? "")) "Should reject empty string")
    (is (false? (main/valid-input? " ")) "Should reject whitespace")
    (is (false? (main/valid-input? nil)) "Should reject nil input"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;      get-valid-input-testing        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest get-valid-input-tests
  (testing "Valid input first try"
    (with-redefs [read-line (constantly "1")]
      (is (= 1 (main/get-valid-input "Enter a number:" main/valid-input?))
          "Should return 1 when input is '1'")))

  (testing "Valid input after invalid attempts"
    (let [inputs (atom ["abc" "3" "-1" "2"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                (swap! inputs rest)
                                next-input)
                    println identity]  ; Suppress output during test
        (is (= 2 (main/get-valid-input "Enter a number:" main/valid-input?))
            "Should return 2 after receiving invalid inputs"))))

  (testing "Empty string followed by valid input"
    (let [inputs (atom ["" "1"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                (swap! inputs rest)
                                next-input)
                    println identity]
        (is (= 1 (main/get-valid-input "Enter a number:" main/valid-input?))
            "Should handle empty string and return 1"))))

  (testing "Whitespace followed by valid input"
    (let [inputs (atom [" " "0"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                (swap! inputs rest)
                                next-input)
                    println identity]
        (is (= 0 (main/get-valid-input "Enter a number:" main/valid-input?))
            "Should handle whitespace and return 0"))))

  (testing "Multiple invalid numeric inputs"
    (let [inputs (atom ["5" "4" "3" "2"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                (swap! inputs rest)
                                next-input)
                    println identity]
        (is (= 2 (main/get-valid-input "Enter a number:" main/valid-input?))
            "Should handle multiple invalid numbers before valid input"))))

  (testing "Special characters followed by valid input"
    (let [inputs (atom ["@#$" "1"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                (swap! inputs rest)
                                next-input)
                    println identity]
        (is (= 1 (main/get-valid-input "Enter a number:" main/valid-input?))
            "Should handle special characters and return 1"))))

  (testing "Decimal numbers followed by valid input"
    (let [inputs (atom ["1.5" "2.0" "1"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                (swap! inputs rest)
                                next-input)
                    println identity]
        (is (= 1 (main/get-valid-input "Enter a number:" main/valid-input?))
            "Should handle decimal numbers and return 1"))))

  (testing "Mixed invalid inputs"
    (let [inputs (atom ["abc" "3.14" "-1" "@#$" "2"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                (swap! inputs rest)
                                next-input)
                    println identity]
        (is (= 2 (main/get-valid-input "Enter a number:" main/valid-input?))
            "Should handle mixed invalid inputs before valid input"))))

  (testing "Valid input with custom validation function"
    (let [custom-validator (fn [input] 
                           (try
                            (= (Integer/parseInt input) 42)
                            (catch NumberFormatException _ false)))]
      (with-redefs [read-line (constantly "42")]
        (is (= 42 (main/get-valid-input "Enter 42:" custom-validator))
            "Should work with custom validation function"))))

  (testing "Leading/trailing whitespace with valid input"
    (let [inputs (atom [" 1 " "2"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                (swap! inputs rest)
                                next-input)
                    println identity]
        (is (= 2 (main/get-valid-input "Enter a number:" main/valid-input?))
            "Should handle inputs with leading/trailing whitespace")))))

(run-tests)