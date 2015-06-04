(ns lambda.de-bruijn-test
  (:require [clojure.test :refer :all]
            [lambda.parser :refer :all]
            [lambda.de-bruijn :refer :all]
            [lambda.terms :refer :all]))

(defn remove-names-parse [term]
  (->> term
       parse-term
       remove-names
       db-stringify-term))

(deftest remove-names-test
  (testing "remove-names"
    (is (= "0" (remove-names-parse "x")))
    (is (= "(0 (1 2))" (remove-names-parse "(x (y z))")))
    (is (= "(lambda (lambda 1))" (remove-names-parse "(lambda x (lambda y x))")))
    (is (= "(lambda (lambda 0))" (remove-names-parse "(lambda x (lambda y y))")))
    (is (= "(lambda (lambda 2))" (remove-names-parse "(lambda x (lambda y z))")))
    (is (= "(lambda (lambda (lambda ((2 0) (1 0)))))" (remove-names-parse "(lambda x (lambda y (lambda z ((x z) (y z)))))")))
    (is (= "(lambda ((lambda (0 (lambda 0))) (lambda (1 0))))" (remove-names-parse "(lambda z ((lambda y (y (lambda x x))) (lambda x (z x))))" )))))

(defn add-names-parse [term]
  (->> term
       db-parse-term
       add-names
       stringify-term))

(deftest add-names-test
  (testing "add-names"
    (is (= "x0" (add-names-parse "0")))
    (is (= "(x0 (x1 x2))" (add-names-parse "(0 (1 2))")))
    (is (= "(lambda a (lambda b a))" (add-names-parse "(lambda (lambda 1))")))
    (is (= "(lambda a (lambda b b))" (add-names-parse "(lambda (lambda 0))")))
    (is (= "(lambda a (lambda b x3))" (add-names-parse "(lambda (lambda 5))")))
    (is (= "(lambda a (lambda b (lambda c ((a c) (b c)))))" (add-names-parse "(lambda (lambda (lambda ((2 0) (1 0)))))")))
    (is (= "(lambda a ((lambda b (b (lambda c c))) (lambda b (a b))))" (add-names-parse "(lambda ((lambda (0 (lambda 0))) (lambda (1 0))))" )))))
