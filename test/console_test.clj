(ns console-test
  (:require [clojure.test :refer :all]
            [tic-tac-cloj.console :as console]))

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
                                        ;       valid-input? Testing          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest valid-input-tests
  (testing "Valid numeric inputs"
    (is (true? (console/valid-input? "0")) "Should accept '0'")
    (is (true? (console/valid-input? "1")) "Should accept '1'")
    (is (true? (console/valid-input? "2")) "Should accept '2'"))

  (testing "Non-numeric inputs"
    (is (false? (console/valid-input? "abc")) "Should reject alphabetic input")
    (is (false? (console/valid-input? "1.5")) "Should reject decimal numbers")
    (is (false? (console/valid-input? "")) "Should reject empty string")
    (is (false? (console/valid-input? " ")) "Should reject whitespace")
    (is (false? (console/valid-input? nil)) "Should reject nil input")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;      get-valid-input-testing        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest get-valid-input-tests
  (testing "Valid input first try"
    (with-redefs [read-line (constantly "1")]
      (is (= 1 (console/get-valid-input "Enter a number:" console/valid-input?))
          "Should return 1 when input is '1'")))

  (testing "Valid input after invalid attempts"
    (let [inputs (atom ["abc" "3" "-1" "2"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                 (swap! inputs rest)
                                 next-input)
                    println identity]
        (is (= 2 (console/get-valid-input "Enter a number:" console/valid-input?))
            "Should return 2 after receiving invalid inputs"))))

  (testing "Empty string followed by valid input"
    (let [inputs (atom ["" "1"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                 (swap! inputs rest)
                                 next-input)
                    println identity]
        (is (= 1 (console/get-valid-input "Enter a number:" console/valid-input?))
            "Should handle empty string and return 1"))))

  (testing "Whitespace followed by valid input"
    (let [inputs (atom [" " "0"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                 (swap! inputs rest)
                                 next-input)
                    println identity]
        (is (= 0 (console/get-valid-input "Enter a number:" console/valid-input?))
            "Should handle whitespace and return 0"))))

  (testing "Multiple invalid numeric inputs"
    (let [inputs (atom ["5" "4" "3" "2"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                 (swap! inputs rest)
                                 next-input)
                    println identity]
        (is (= 2 (console/get-valid-input "Enter a number:" console/valid-input?))
            "Should handle multiple invalid numbers before valid input"))))

  (testing "Special characters followed by valid input"
    (let [inputs (atom ["@#$" "1"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                 (swap! inputs rest)
                                 next-input)
                    println identity]
        (is (= 1 (console/get-valid-input "Enter a number:" console/valid-input?))
            "Should handle special characters and return 1"))))

  (testing "Decimal numbers followed by valid input"
    (let [inputs (atom ["1.5" "2.0" "1"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                 (swap! inputs rest)
                                 next-input)
                    println identity]
        (is (= 1 (console/get-valid-input "Enter a number:" console/valid-input?))
            "Should handle decimal numbers and return 1"))))

  (testing "Mixed invalid inputs"
    (let [inputs (atom ["abc" "3.14" "-1" "@#$" "2"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                 (swap! inputs rest)
                                 next-input)
                    println identity]
        (is (= 2 (console/get-valid-input "Enter a number:" console/valid-input?))
            "Should handle mixed invalid inputs before valid input"))))

  (testing "Valid input with custom validation function"
    (let [custom-validator (fn [input]
                             (try
                               (= (Integer/parseInt input) 42)
                               (catch NumberFormatException _ false)))]
      (with-redefs [read-line (constantly "42")]
        (is (= 42 (console/get-valid-input "Enter 42:" custom-validator))
            "Should work with custom validation function"))))

  (testing "Leading/trailing whitespace with valid input"
    (let [inputs (atom [" 1 " "2"])]
      (with-redefs [read-line #(let [next-input (first @inputs)]
                                 (swap! inputs rest)
                                 next-input)
                    println identity]
        (is (= 2 (console/get-valid-input "Enter a number:" console/valid-input?))
            "Should handle inputs with leading/trailing whitespace")))))

(run-tests)
