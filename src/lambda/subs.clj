(ns lambda.subs
  (:require [lambda.terms :refer :all]))

(defn substitute [variable substitution term]
  (case (:type term)
    :variable (if (= variable term)
                  substitution
                  term)
    :application (make-application
                   (substitute variable substitution (:func term))
                   (substitute variable substitution (:arg term)))
    :abstraction (if (not= variable (:var term))
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
    :abstraction (if (= old-var (:var term))
                     (make-abstraction
                       new-var
                       (rename-bound-var
                         old-var
                         new-var
                         (substitute old-var new-var (:body term))))
                     (make-abstraction
                       (:var term)
                       (rename-bound-var old-var new-var (:body term))))))
