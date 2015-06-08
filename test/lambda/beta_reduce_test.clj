(ns lambda.beta-reduce-test
  (:require [clojure.test :refer :all]
            [clojure.core.strint :refer [<<]]
            [lambda.parser :refer :all]
            [lambda.beta-reduce :refer :all]))

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
    (let [K "(lambda x (lambda y x))"
          I "(lambda x x)"
          w "(lambda x (x x))"
          Omega (<< "(~{w} ~{w})")
          c (fn [n]
              (format "(lambda f (lambda x %s))"
                (loop [n n term "x"]
                  (if (zero? n)
                      term
                      (recur (dec n) (format "(f %s)" term))))))
          cs "(lambda n (lambda f (lambda x ((n f) (f x)))))"
          c+ (<< "(lambda m (lambda n ((m ~{cs}) n)))")
          c* (<< "(lambda m (lambda n ((m (~{c+} n)) ~(c 0))))")
          cexp (<< "(lambda m (lambda n ((n (~{c*} m)) ~(c 1))))")
          True "(lambda x (lambda y x))"
          False "(lambda x (lambda y y))"
          And "(lambda p (lambda q ((p q) p)))"
          Pair "(lambda x (lambda y (lambda z ((z x) y))))"
          Left (<< "(lambda p (p ~{True}))")
          Right (<< "(lambda p (p ~{False}))")
          cp (<< "(lambda n (~{Right} ((n (lambda p ((~{Pair} (~{cs} (~{Left} p))) (~{Left} p)))) ((~{Pair} ~(c 0)) ~(c 0)))))")
          c0? (<< "(lambda n ((n (lambda x ~{False})) ~{True}))")
          Y "(lambda f ((lambda x (f (x x))) (lambda x (f (x x)))))"
          Sum (<< "(~{Y} (lambda f (lambda n (((~{c0?} n) n) ((~{c+} n) (f (~{cp} n)))))))")]
      (is (= "x" (normalize-parse "x")))
      (is (= I (normalize-parse (<< "((~{K} ~{I}) ~{Omega})"))))
      (is (= (c 1) (normalize-parse (<< "(~{cs} ~(c 0))"))))
      (is (= (c 4) (normalize-parse (<< "(~{cs} ~(c 3))"))))
      (is (= (c 5) (normalize-parse (<< "((~{c+} ~(c 2)) ~(c 3))"))))
      (is (= (c 42) (normalize-parse (<< "((~{c*} ~(c 6)) ~(c 7))"))))
      (is (= (c 81) (normalize-parse (<< "((~{cexp} ~(c 3)) ~(c 4))"))))
      (is (= False (normalize-parse (<< "((~{And} ~{True}) ~{False})"))))
      (is (= False (normalize-parse (<< "((~{And} ~{False}) ~{True})"))))
      (is (= True (normalize-parse (<< "((~{And} ~{True}) ~{True})"))))
      (is (= K (normalize-parse (<< "(~{Left} ((~{Pair} ~{K}) ~{I}))"))))
      (is (= I (normalize-parse (<< "(~{Right} ((~{Pair} ~{K}) ~{I}))"))))
      (is (= (c 0) (normalize-parse (<< "(~{cp} ~(c 0))"))))
      (is (= (c 0) (normalize-parse (<< "(~{cp} ~(c 1))"))))
      (is (= (c 49) (normalize-parse (<< "(~{cp} ~(c 50))"))))
      (is (= True (normalize-parse (<< "(~{c0?} ~(c 0))"))))
      (is (= False (normalize-parse (<< "(~{c0?} ~(c 10))"))))
      (is (= (c 0) (normalize-parse (<< "(~{Sum} ~(c 0))"))))
      (is (= (c 1) (normalize-parse (<< "(~{Sum} ~(c 1))"))))
      (is (= (c 21) (normalize-parse (<< "(~{Sum} ~(c 6))")))))))
