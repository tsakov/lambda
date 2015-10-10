(ns lambda.parser
  (:require [clojure.core.match :refer [match]]
            [lambda.terms :refer :all]
            [lambda.constants :refer :all]))

(declare constant?)
(declare get-constant)
(declare expand-abstractions)
(declare expand-applications)

;;; x -> variable
;;; (M N) -> application
;;; (lambda x M) -> abstraction
(defn parse-term [input]
  (if (string? input)
      (parse-term (read-string input))
      (match input
        (c :guard constant?) (parse-term (get-constant c))
        (x :guard symbol?) (make-variable x)
        (n :guard integer?) (parse-term (c n))
        ([(func :guard #(not= 'lambda %)) arg] :seq) (make-application (parse-term func) (parse-term arg))
        (['lambda (variable :guard symbol?) body] :seq) (make-abstraction variable (parse-term body))
        (['lambda (variables :guard list?) body] :seq) (parse-term (expand-abstractions input))
        ([func & args] :seq) (parse-term (expand-applications input)))))

(defn constant? [sym]
  (contains? (ns-publics 'lambda.constants) sym))

(defn get-constant [sym]
  @(ns-resolve 'lambda.constants sym))

(defn expand-applications [term]
  (reduce list term))

(defn expand-abstractions [term]
  (let [[_ vars body] term]
    (reduce #(list 'lambda %2 %1)
            body
            (reverse vars))))

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
