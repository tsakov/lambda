(ns lambda.beta-reduce
  (:require [lambda.terms :refer :all]
            [lambda.parser :refer :all]
            [lambda.subs :refer :all]))

(defn redex? [term]
  (and (= (:type term) :application)
       (= (:type (:func term)) :abstraction)))

(defn normal-form? [term]
  (case (:type term)
    :variable true
    :application (and (not (redex? term))
                      (normal-form? (:func term))
                      (normal-form? (:arg term)))
    :abstraction (normal-form? (:body term))))

(defn beta-reduce [term]
  (if (redex? term)
      (safe-subs (make-variable (:var (:func term)))
                 (:arg term)
                 (:body (:func term)))
      term))

(defn left-beta-reduce [term]
  (case (:type term)
    :variable term
    :application (cond
                   (redex? term) (beta-reduce term)
                   (not (normal-form? (:func term))) (make-application
                                                       (left-beta-reduce (:func term))
                                                       (:arg term))
                   (not (normal-form? (:arg term))) (make-application
                                                      (:func term)
                                                      (left-beta-reduce (:arg term)))
                   :else term)
    :abstraction (make-abstraction
                   (:var term)
                   (left-beta-reduce (:body term)))))

(defn normalize
  ([term]
    (normalize term left-beta-reduce))
  ([term reducer]
    (if (normal-form? term)
        term
        (normalize (reducer term) reducer))))

(defn normalize-trace [term]
  (normalize term
             (fn [term]
               (print-term term)
               (println)
               (left-beta-reduce term))))
