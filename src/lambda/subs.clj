(ns lambda.subs
  (:require [clojure.set :refer [union intersection]])
  (:require [lambda.terms :refer :all]))

(defn substitute [variable substitution term]
  (case (:type term)
    :variable (if (= variable term)
                  substitution
                  term)
    :application (make-application
                   (substitute variable substitution (:func term))
                   (substitute variable substitution (:arg term)))
    :abstraction (if (not= (:name variable) (:var term))
                     (make-abstraction
                       (:var term)
                       (substitute variable substitution (:body term)))
                     term)))

(defn rename-bound-var [old-var new-var term]
  (case (:type term)
    :variable term
    :application (make-application
                   (rename-bound-var old-var new-var (:func term))
                   (rename-bound-var old-var new-var (:arg term)))
    :abstraction (if (= (:name old-var) (:var term))
                     (make-abstraction
                       (:name new-var)
                       (rename-bound-var
                         old-var
                         new-var
                         (substitute old-var new-var (:body term))))
                     (make-abstraction
                       (:var term)
                       (rename-bound-var old-var new-var (:body term))))))

(def var-names
  (let [alphabet (map (comp symbol str char) (range (int \a) (inc (int \z))))]
    alphabet))

(defn new-var-name [& terms]
  (first (remove (apply union (map vars terms)) var-names)))

(defn rename-conflict-vars [substitution term]
  (if-let [conflict-var-name (first (intersection (bound-vars term)
                                                  (free-vars substitution)))]
    (let [new-var (make-variable (new-var-name substitution term))
          conflict-var (make-variable conflict-var-name)
          new-term (rename-bound-var conflict-var new-var term)]
      (rename-conflict-vars substitution new-term))
    term))

(defn safe-subs [variable substitution term]
  (substitute variable substitution (rename-conflict-vars substitution term)))
