(ns lambda.parser-test
  (:require [clojure.test :refer :all]
            [lambda.terms :refer :all]
            [lambda.parser :refer :all]))

(deftest parse-term-test
  (testing "parse-term"
    (is (= (parse-term "x")
           (make-variable 'x)))
    (is (= (parse-term "(x y)")
           (make-application
             (make-variable 'x)
             (make-variable 'y))))
    (is (= (parse-term "(lambda x y)")
           (make-abstraction
             'x
             (make-variable 'y)))))

  (testing "parse-term nested terms"
    (is (= (parse-term "(x (y z))")
           (make-application
             (make-variable 'x)
             (make-application
               (make-variable 'y)
               (make-variable 'z)))))
    (is (= (parse-term "((x y) z)")
           (make-application
             (make-application
               (make-variable 'x)
               (make-variable 'y))
             (make-variable 'z))))
    (is (= (parse-term "(lambda x (lambda y (x y)))")
           (make-abstraction
             'x
             (make-abstraction
               'y
               (make-application
                 (make-variable 'x)
                 (make-variable 'y)))))))
  (testing "parse-term multiple arguments"
    (is (= (parse-term "(x y z t)")
           (parse-term "(((x y) z) t)")))
    (is (= (parse-term "(lambda (x y z) (y z x))")
           (parse-term "(lambda x (lambda y (lambda z ((y z) x))))")))
    (is (= (parse-term "4")
           (parse-term "(lambda f (lambda x (f (f (f (f x))))))")))
    (is (= (parse-term "True")
           (parse-term "(lambda x (lambda y x))")))))

(deftest stringify-term-test
  (testing "stringify-term"
    (let [terms ["x"
                 "(x y)"
                 "(lambda x y)"
                 "(x (y z))"
                 "((x y) z)"
                 "(lambda x (lambda y (x y)))"]]
      (doseq [term terms]
        (is (= term
               (stringify-term (parse-term term))))))))
