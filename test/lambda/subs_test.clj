(ns lambda.subs-test
  (:require [clojure.test :refer :all]
            [lambda.parser :refer :all]
            [lambda.subs :refer :all]))

(defn subs-parse [variable substitution term]
  (->> [variable substitution term]
       (map parse-term)
       (apply substitute)
       stringify-term))

(deftest substitute-test
  (testing "substitute"
    (is (= "y" (subs-parse "x" "y" "x")))
    (is (= "((y z) ((y z) z))" (subs-parse "x" "(y z)" "(x (x z))")))
    (is (= "(lambda x x)" (subs-parse "x" "y" "(lambda x x)")))
    (is (= "(y (lambda x x))" (subs-parse "x" "y" "(x (lambda x x))")))))

(defn rename-parse [old-var new-var term]
  (->> [old-var new-var term]
       (map parse-term)
       (apply rename-bound-var)
       stringify-term))

(deftest rename-bound-var-test
  (testing "rename-bound-var"
    (is (= "x" (rename-parse "x" "y" "x")))
    (is (= "(lambda y y)" (rename-parse "x" "y" "(lambda x x)")))
    (is (= "((x z) (lambda y (y y)))" (rename-parse "x" "y" "((x z) (lambda x (x x)))")))
    (is (= "(lambda z (lambda y (y z)))" (rename-parse "x" "y" "(lambda z (lambda x (x z)))")))
    (is (= "(lambda y (lambda y (y z)))" (rename-parse "x" "y" "(lambda x (lambda x (x z)))")))))

(defn rename-conflict-parse [substitution term]
  (->> [substitution term]
       (map parse-term)
       (apply rename-conflict-vars)
       stringify-term))

(deftest rename-conflict-vars-test
  (testing "rename-conflict-vars"
    (is (= "(lambda a (a x))" (rename-conflict-parse "y" "(lambda y (y x))")))
    (is (= "(lambda a (lambda b x))" (rename-conflict-parse "((x y) z)" "(lambda y (lambda z x))")))))

(defn safe-subs-parse [variable substitution term]
  (->> [variable substitution term]
       (map parse-term)
       (apply safe-subs)
       stringify-term))

(deftest safe-subs-test
  (testing "safe-subs"
    (is (= "(lambda a y)" (safe-subs-parse "x" "y" "(lambda y x)")))
    (is (= "(lambda a (lambda b (y z)))" (safe-subs-parse "x" "(y z)" "(lambda y (lambda z x))")))))
