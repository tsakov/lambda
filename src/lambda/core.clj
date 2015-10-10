(ns lambda.core
  (:gen-class)
  (:require [lambda.parser :refer :all]
            [lambda.beta-reduce :refer :all])
  (:import jline.console.ConsoleReader))

(def prompt "=> ")

(defn -main
  "Start the lambda REPL."
  [& args]
  (println "lambda 0.1.0")
  (let [cr (ConsoleReader.)]
    (while true
      (-> (.readLine cr prompt)
          parse-term
          normalize
          stringify-term
          println))))
