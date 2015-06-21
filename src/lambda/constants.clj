(ns lambda.constants
  (:require [clojure.core.strint :refer [<<]]))

(def K "(lambda x (lambda y x))")
(def I "(lambda x x)")
(def w "(lambda x (x x))")
(def Omega (<< "(~{w} ~{w})"))
(def Y "(lambda f ((lambda x (f (x x))) (lambda x (f (x x)))))")

(def True "(lambda x (lambda y x))")
(def False "(lambda x (lambda y y))")
(def And "(lambda p (lambda q ((p q) p)))")
(def Or "(lambda p (lambda q ((p p) q)))")
(def Not (<< "(lambda p ((p ~{False}) ~{True}))"))

(def Pair "(lambda x (lambda y (lambda z ((z x) y))))")
(def Left (<< "(lambda p (p ~{True}))"))
(def Right (<< "(lambda p (p ~{False}))"))

(defn c [n]
  (format "(lambda f (lambda x %s))"
    (loop [n n term "x"]
      (if (zero? n)
          term
          (recur (dec n) (format "(f %s)" term))))))

(def c0? (<< "(lambda n ((n (lambda x ~{False})) ~{True}))"))
(def cs "(lambda n (lambda f (lambda x ((n f) (f x)))))")
(def cp (<< "(lambda n (~{Right} ((n (lambda p ((~{Pair} (~{cs} (~{Left} p))) (~{Left} p)))) ((~{Pair} ~(c 0)) ~(c 0)))))"))
(def c+ (<< "(lambda m (lambda n ((m ~{cs}) n)))"))
(def c- (<< "(lambda m (lambda n ((n ~{cp}) m)))"))
(def c* (<< "(lambda m (lambda n ((m (~{c+} n)) ~(c 0))))"))
(def cexp (<< "(lambda m (lambda n ((n (~{c*} m)) ~(c 1))))"))
(def chyp (<< "(lambda m (lambda n ((n (~{cexp} m)) ~(c 1))))"))

(def c<= (<< "(lambda m (lambda n (~{c0?} ((~{c-} m) n))))"))
(def c= (<< "(lambda m (lambda n ((~{And} ((~{c<=} m) n)) ((~{c<=} n) m))))"))
(def c< (<< "(lambda m (lambda n (~{Not} ((~{c<=} n) m))))"))

(def Quot
  (<< "(~{Y} (lambda f (lambda m (lambda n ((((~{c<} m) n) ~(c 0)) (~{cs} ((f ((~{c-} m) n)) n)))))))"))

(def Rem
  (<< "(~{Y} (lambda f (lambda m (lambda n ((((~{c<} m) n) m) ((f ((~{c-} m) n)) n))))))"))

(def Div? (<< "(lambda m (lambda n (~{c0?} ((~{Rem} n) m))))"))

(def Prime?
  (<< "(lambda n ((~{And} ((~{c<=} ~(c 2)) n))
                    (~{Not} (~{Right}
                            ((((~{c-} n) ~(c 2))
                              (lambda p ((~{Pair} (~{cs} (~{Left} p))) ((~{Or} (~{Right} p)) ((~{Div?} (~{cs} (~{Left} p))) n)))))
                             ((~{Pair} ~(c 1)) ~{False}))))))"))

(def Sum (<< "(~{Y} (lambda f (lambda n (((~{c0?} n) n) ((~{c+} n) (f (~{cp} n)))))))"))

(def Nil False)
(def First Left)
(def Rest Right)
(def Empty? (<< "(lambda l ((l (lambda h (lambda t (lambda d ~{False})))) ~{True}))"))

(defn List [& numbers]
  (->> numbers
       (map c)
       (reduce (fn [l n] (<< "((~{Pair} ~{n}) ~{l})")) Nil)))

(def Map
  (<< "(~{Y}
        (lambda m
          (lambda f
            (lambda l
              (((~{Empty?} l) ~{Nil})
                              ((~{Pair} (f (~{First} l)))
                                        ((m f) (~{Rest} l))))))))"))

(def Foldr
  (<< "(~{Y}
          (lambda fld
            (lambda f
              (lambda v
                (lambda l
                  (((~{Empty?} l) v)
                                  ((f (~{First} l))
                                     (((fld f) v) (~{Rest} l)))))))))"))

(def Filter
  (<< "(~{Y}
          (lambda f
            (lambda p
              (lambda l
                (((~{Empty?} l) ~{Nil})
                                (((p (~{First} l))
                                  ((~{Pair} (~{First} l))
                                            ((f p) (~{Rest} l))))
                                  ((f p) (~{Rest} l))))))))"))
