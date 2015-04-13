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
                 (make-variable 'y))))))))
