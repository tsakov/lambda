(ns lambda.parser
  (:require [clojure.core.match :refer [match]]
            [lambda.terms :refer :all]))

;;; x -> variable
;;; (M N) -> application
;;; (lambda x M) -> abstraction
(defn parse-term [input]
  (if (string? input)
      (parse-term (read-string input))
      (match input
        (x :guard symbol?) (make-variable x)
        ([func arg] :seq) (make-application (parse-term func) (parse-term arg))
        (['lambda (variable :guard symbol?) body] :seq) (make-abstraction variable (parse-term body)))))

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
