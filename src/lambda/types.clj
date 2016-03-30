(ns lambda.types
  (:require [clojure.core.logic :refer :all
                                :rename {== ===}]))

(declare typeo)

(defn typeof
  ([term]
    (typeof term []))
  ([term context]
    (first
      (run 1 [type]
        (typeo term type context)))))

(defn typeo [term type context]
  (conde
    [(=== (:type term) :variable)
     (membero [(:name term) type] context)]

    [(=== (:type term) :application)
     (fresh [t]
       (typeo (:func term) [t '-> type] context)
       (typeo (:arg term) t context))]

    [(=== (:type term) :abstraction)
     (fresh [t1 t2 c]
       (=== type [t1 '-> t2])
       (conso [(:var term) t1] context c)
       (typeo (:body term) t2 c))]))
