(ns lambda.terms
  (:require [clojure.set :refer [union]]))

(defn make-variable [var-name]
  {:type :variable
   :name var-name})

(defn make-abstraction [variable term]
  {:type :abstraction
   :var variable
   :body term})

(defn make-application [function argument]
  {:type :application
   :func function
   :arg argument})

(defn free-vars [term]
  (case (:type term)
    :variable #{(:var term)}
    :application (union (free-vars (:func term))
                        (free-vars (:arg term)))
    :abstraction (disj (free-vars (:body term))
                       (:var term))))

(defn bound-vars [term]
  (case (:type term)
    :variable #{}
    :application (union (free-vars (:func term))
                        (free-vars (:arg term)))
    :abstraction (conj (free-vars (:body term))
                       (:var term))))

(defn vars [term]
  (union (free-vars term)
         (bound-vars term)))
