(ns lambda.de-bruijn
  (:require [lambda.terms :refer :all]))

(defn db-make-variable [ind]
  {:type :variable
   :index ind})

(defn db-make-abstraction [term]
  {:type :abstraction
   :body term})

(defn db-make-application [function argument]
  {:type :application
   :func function
   :arg argument})

(defn db-parse-term [input]
  (cond
    (string? input) (db-parse-term (read-string input))
    (number? input) (db-make-variable input)
    (list? input)
      (let [[a b] input]
        (if (= a 'lambda)
            (db-make-abstraction (db-parse-term b))
            (db-make-application (db-parse-term a) (db-parse-term b))))))

(defn db-stringify-term [term]
  (case (:type term)
    :variable (str (:index term))
    :application (format "(%s %s)"
                   (db-stringify-term (:func term))
                   (db-stringify-term (:arg term)))
    :abstraction (format "(lambda %s)"
                   (db-stringify-term (:body term)))))

(defn create-context [term]
  (vec (free-vars term)))

(defn get-index [variable context]
  (.indexOf context variable))

(defn get-name [ind context]
  (nth context ind))

(defn add-index
  "Substitute variable with ind in term."
  [variable ind term]
  (case (:type term)
    :variable (if (= variable (:name term))
                  (db-make-variable ind)
                  term)
    :application (db-make-application
                   (add-index variable ind (:func term))
                   (add-index variable ind (:arg term)))
    :abstraction (if (not= (:name variable) (:var term))
                     (make-abstraction (:var term) (add-index variable  (inc ind) (:body term)))
                     term)))

(defn remove-names
  ([term]
   (remove-names term (create-context term)))
  ([term context]
   (remove-names term context 0))
  ([term context level]
   (case (:type term)
     :variable (if (:name term)
                   (db-make-variable (+ (get-index (:name term) context) level))
                   term)
     :application (db-make-application
                    (remove-names (:func term) context level)
                    (remove-names (:arg term) context level))
     :abstraction (db-make-abstraction
                    (remove-names (add-index (:var term) 0 (:body term))
                                  context
                                  (inc level))))))

(defn index->name [ind variable term]
  (case (:type term)
    :variable (if (= ind (:index term))
                  (make-variable variable)
                  term)
    :application (make-application
                   (index->name ind variable (:func term))
                   (index->name ind variable (:arg term)))
    :abstraction (db-make-abstraction
                   (index->name (inc ind) variable (:body term)))))

(defn add-names
  ([term]
   (add-names term (map (comp symbol str char) (range (int \a) (inc (int \z))))))
  ([term context]
   (add-names term context 0))
  ([term context level]
   (case (:type term)
     :variable (if (:index term)
                   (make-variable (symbol (str 'x (- (:index term) level))))
                   term)
     :application (make-application
                    (add-names (:func term) context level)
                    (add-names (:arg term) context level))
     :abstraction (make-abstraction
                    (first context)
                    (add-names
                      (index->name 0 (first context) (:body term))
                      (rest context)
                      (inc level))))))
