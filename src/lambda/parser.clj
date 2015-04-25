(ns lambda.parser
  (:require [lambda.terms :refer :all]))

;;; x -> variable
;;; (M N) -> application
;;; (lambda x M) -> abstraction
(defn parse-term [input]
  (cond
    (string? input) (parse-term (read-string input))
    (symbol? input) (make-variable input)
    (list? input)
      (let [[a b c] input]
        (case (count input)
          2 (make-application (parse-term a) (parse-term b))
          3 (make-abstraction b (parse-term c))
          nil))))

(defn stringify-term [term]
  (case (:type term)
    :variable (str (:name term))
    :application (format "(%s %s)"
                   (stringify-term (:func term))
                   (stringify-term (:arg term)))
    :abstraction (format "(lambda %s %s)"
                   (:var term)
                   (stringify-term (:body term)))))

(defn print-term [term]
  (println (stringify-term term)))