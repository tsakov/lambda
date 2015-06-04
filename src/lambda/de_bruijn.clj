(ns lambda.de-bruijn
  (:require [clojure.core.match :refer [match]]
            [lambda.terms :refer :all]))

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
  (if (string? input)
      (db-parse-term (read-string input))
      (match input
        (x :guard number?) (db-make-variable x)
        (['lambda body] :seq) (db-make-abstraction (db-parse-term body))
        ([func arg] :seq) (db-make-application (db-parse-term func) (db-parse-term arg)))))

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

(defn name->index
  "Substitute variable with ind in term."
  [variable ind term]
  (case (:type term)
    :variable (if (= variable (:name term))
                  (db-make-variable ind)
                  term)
    :application (db-make-application
                   (name->index variable ind (:func term))
                   (name->index variable ind (:arg term)))
    :abstraction (if (not= variable (:var term))
                     (make-abstraction (:var term) (name->index variable (inc ind) (:body term)))
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
                    (remove-names (name->index (:var term) 0 (:body term))
                                  context
                                  (inc level))))))

(defn index->name
  "Substitute ind with variable in term."
  [ind variable term]
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

(defn shift
  ([term]
   (shift term 1))
  ([term offset]
   (shift term offset 0))
  ([term offset start]
   (case (:type term)
     :variable (if (< (:index term) start)
                   term
                   (db-make-variable (+ (:index term) offset)))
     :application (db-make-application
                    (shift (:func term) offset start)
                    (shift (:arg term) offset start))
     :abstraction (db-make-abstraction
                    (shift (:body term) offset (inc start))))))

(defn db-subs [ind substitution term]
  (case (:type term)
    :variable (if (= ind (:index term))
                  substitution
                  term)
    :application (db-make-application
                   (db-subs ind substitution (:func term))
                   (db-subs ind substitution (:arg term)))
    :abstraction (db-make-abstraction
                   (db-subs (inc ind) (shift substitution) (:body term)))))
