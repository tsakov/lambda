(ns lambda.core
  (:gen-class)
  (:require [clojure.string :refer [split]]
            [lambda.parser :refer :all]
            [lambda.beta-reduce :refer :all]
            [lambda.types :refer [typeof]])
  (:import jline.console.ConsoleReader))

(def prompt "=> ")
(def error-message
  (str "Illegal expression! Syntax: <command> <term>\n"
       "Available commands: print | reduce | reduce1 | trace | type"))

(def last-result (atom "_"))
(defn update-last-result [term]
  (swap! last-result (fn [_] term)))

(defn -main
  "Start the lambda REPL."
  [& args]
  (println "lambda 0.1.0")
  (let [cr (ConsoleReader.)]
    (while true
      (let [[cmd term] (split (.readLine cr prompt) #"\s+" 2)
            term (if (= term "_") @last-result term)]
        (case cmd
          "print" (-> term parse-term stringify-term update-last-result println)
          "reduce" (-> term parse-term normalize stringify-term update-last-result println)
          "reduce1" (-> term parse-term left-beta-reduce stringify-term update-last-result println)
          "trace" (-> term parse-term normalize-trace stringify-term update-last-result println)
          "type" (-> term parse-term typeof println)
          (println error-message))))))
