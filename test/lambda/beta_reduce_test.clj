(ns lambda.beta-reduce-test
  (:require [clojure.test :refer :all]
            [clojure.core.strint :refer [<<]]
            [lambda.parser :refer :all]
            [lambda.beta-reduce :refer :all]
            [lambda.constants :refer :all]))

(defn redex-parse [term]
  (-> term parse-term redex?))

(deftest redex-test
  (testing "redex?"
    (is (not (redex-parse "x")))
    (is (not (redex-parse "(x (x y))")))
    (is (not (redex-parse "(lambda x (x (lambda y x)))")))
    (is (redex-parse "((lambda x x) y)"))
    (is (redex-parse "((lambda x (x y)) (lambda y (y x)))"))))

(defn normal-form-parse [term]
  (-> term parse-term normal-form?))

(deftest normal-form-test
  (testing "normal-form?"
    (is (normal-form-parse "x"))
    (is (normal-form-parse "(x (x y))"))
    (is (normal-form-parse "(lambda x (x (lambda y x)))"))
    (is (not (normal-form-parse "((lambda x x) y)")))
    (is (not (normal-form-parse "(lambda x (x (lambda y (x ((lambda z z) x)))))")))))

(defn beta-reduce-parse [term]
  (-> term parse-term beta-reduce stringify-term))

(deftest beta-reduce-test
  (testing "beta-reduce"
    (is (= "x" (beta-reduce-parse "x")))
    (is (= "(x ((lambda y y) x))" (beta-reduce-parse "(x ((lambda y y) x))")))
    (is (= "((y y) (y y))" (beta-reduce-parse "((lambda x (x x)) (y y))")))))

(defn normalize-parse [term]
  (-> term parse-term normalize stringify-term))

(deftest normalize-test
  (testing "normalize"
    (is (= "x" (normalize-parse "x")))
    (is (= I (normalize-parse (<< "((~{K} ~{I}) ~{Omega})"))))
    (is (= (c 1) (normalize-parse (<< "(~{cs} ~(c 0))"))))
    (is (= (c 4) (normalize-parse (<< "(~{cs} ~(c 3))"))))
    (is (= (c 5) (normalize-parse (<< "((~{c+} ~(c 2)) ~(c 3))"))))
    (is (= (c 2) (normalize-parse (<< "((~{c-} ~(c 5)) ~(c 3))"))))
    (is (= (c 0) (normalize-parse (<< "((~{c-} ~(c 3)) ~(c 3))"))))
    (is (= (c 0) (normalize-parse (<< "((~{c-} ~(c 2)) ~(c 3))"))))
    (is (= (c 42) (normalize-parse (<< "((~{c*} ~(c 6)) ~(c 7))"))))
    (is (= (c 81) (normalize-parse (<< "((~{cexp} ~(c 3)) ~(c 4))"))))
    (is (= False (normalize-parse (<< "((~{And} ~{True}) ~{False})"))))
    (is (= False (normalize-parse (<< "((~{And} ~{False}) ~{True})"))))
    (is (= True (normalize-parse (<< "((~{And} ~{True}) ~{True})"))))
    (is (= True (normalize-parse (<< "((~{c=} ~(c 5)) ~(c 5))"))))
    (is (= False (normalize-parse (<< "((~{c=} ~(c 5)) ~(c 6))"))))
    (is (= True (normalize-parse (<< "((~{c<} ~(c 3)) ~(c 5))"))))
    (is (= False (normalize-parse (<< "((~{c<} ~(c 8)) ~(c 4))"))))
    (is (= K (normalize-parse (<< "(~{Left} ((~{Pair} ~{K}) ~{I}))"))))
    (is (= I (normalize-parse (<< "(~{Right} ((~{Pair} ~{K}) ~{I}))"))))
    (is (= (c 0) (normalize-parse (<< "(~{cp} ~(c 0))"))))
    (is (= (c 0) (normalize-parse (<< "(~{cp} ~(c 1))"))))
    (is (= (c 49) (normalize-parse (<< "(~{cp} ~(c 50))"))))
    (is (= True (normalize-parse (<< "(~{c0?} ~(c 0))"))))
    (is (= False (normalize-parse (<< "(~{c0?} ~(c 10))"))))
    (is (= (c 3) (normalize-parse (<< "((~{Quot} ~(c 15)) ~(c 4))"))))
    (is (= (c 4) (normalize-parse (<< "((~{Quot} ~(c 16)) ~(c 4))"))))
    (is (= (c 4) (normalize-parse (<< "((~{Quot} ~(c 17)) ~(c 4))"))))
    (is (= (c 3) (normalize-parse (<< "((~{Rem} ~(c 15)) ~(c 4))"))))
    (is (= (c 0) (normalize-parse (<< "((~{Rem} ~(c 15)) ~(c 5))"))))
    (is (= False (normalize-parse (<< "((~{Div?} ~(c 7)) ~(c 13))"))))
    (is (= True (normalize-parse (<< "((~{Div?} ~(c 7)) ~(c 14))"))))
    (is (= False (normalize-parse (<< "(~{Prime?} ~(c 0))"))))
    (is (= False (normalize-parse (<< "(~{Prime?} ~(c 1))"))))
    (is (= True (normalize-parse (<< "(~{Prime?} ~(c 2))"))))
    (is (= True (normalize-parse (<< "(~{Prime?} ~(c 7))"))))
    (is (= False (normalize-parse (<< "(~{Prime?} ~(c 9))"))))
    (is (= (c 0) (normalize-parse (<< "(~{Sum} ~(c 0))"))))
    (is (= (c 1) (normalize-parse (<< "(~{Sum} ~(c 1))"))))
    (is (= (c 21) (normalize-parse (<< "(~{Sum} ~(c 6))"))))
    (is (= Nil (normalize-parse (List))))
    (is (= True (normalize-parse (<< "(~{Empty?} ~{Nil})"))))
    (is (= False (normalize-parse (<< "(~{Empty?} ~(List 0))"))))
    (is (= (c 24) (normalize-parse (<< "(((~{Foldr} ~{c*}) ~(c 1)) ~(List 1 2 3 4))"))))
    (is (= (normalize-parse (List 2 4 6 8))
           (normalize-parse (<< "((~{Map} (~{c*} ~(c 2))) ~(List 1 2 3 4))"))))
    (is (= (normalize-parse (List 7 8))
           (normalize-parse (<< "((~{Filter} (~{c<} ~(c 5))) ~(List 1 7 3 8 5))"))))))
