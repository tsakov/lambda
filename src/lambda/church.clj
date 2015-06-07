(ns lambda.church)

(defn cn [n]
  (fn [f]
    (fn [x]
      (loop [n n result x]
        (if (zero? n)
             result
             (recur (dec n) (f result)))))))

(defn cprint [n]
  ((n inc) 0))

(defn cinc [n]
  (fn [f]
    (fn [x]
      (f ((n f) x)))))

(defn c+ [m]
  (fn [n]
    ((m cinc) n)))

(defn c* [m]
  (fn [n]
    ((m (c+ n)) (cn 0))))

(defn cexp [m]
  (fn [n]
    ((n (c* m)) (cn 1))))

(defn chyp [m]
  (fn [n]
    ((n (cexp m)) (cn 1))))

(defn ctrue [x]
  (constantly x))

(defn cfalse [x]
  identity)

(defn cprintp [p]
  ((p true) false))

(defn cnot [p]
  ((p cfalse) ctrue))

(defn cand [p]
  (fn [q]
    ((p q) cfalse)))

(defn cor [p]
  (fn [q]
    ((p ctrue) q)))

(defn cpair [x]
  (fn [y]
    (fn [z]
      ((z x) y))))

(defn cleft [p]
  (p ctrue))

(defn cright [p]
  (p cfalse))

(defn cdec [n]
  (cright
    ((n (fn [p] ((cpair (cinc (cleft p))) (cleft p))))
      ((cpair (cn 0)) (cn 0)))))

(defn c- [m]
  (fn [n]
    ((n cdec) m)))

(defn czero? [n]
  ((n (fn [x] cfalse)) ctrue))

(defn c<= [m]
  (fn [n]
    (czero? ((c- m) n))))

(defn c= [m]
  (fn [n]
    ((cand ((c<= m) n)) ((c<= n) m))))

(defn c< [m]
  (fn [n]
    (cnot ((c<= n) m))))

(defn T [f]
  (let [A (fn [x]
            (f (fn [y]
                 ((x x) y))))]
    (A A)))

(def cfac
  (T (fn [f]
        (fn [n]
          (((czero? n) (cn 1)) (fn [x] (((c* n) (f (cdec n))) x)))))))

(def csum
  (T (fn [f]
       (fn [n]
         (((czero? n) (cn 0)) (fn [x] (((c+ n) (f (cdec n))) x)))))))

(def cquot
  (T (fn [f]
       (fn [m]
         (fn [n]
           ((((c< m) n) (cn 0)) (fn [x] ((cinc ((f ((c- m) n)) n)) x))))))))

(def crem
  (T (fn [f]
       (fn [m]
         (fn [n]
           ((((c< m) n) m) (fn [x] (((f ((c- m) n)) n) x))))))))

(defn cquot1 [m]
  (fn [n]
    (cdec
      ((m
        (fn [q]
          ((((c< m) ((c* n) q)) q) (cinc q))))
        (cn 1)))))

(defn crem1 [m]
  (fn [n]
    ((c- m) ((c* n) ((cquot1 m) n)))))

(defn cdiv? [m]
  (fn [n]
    (czero? ((crem n) m))))

(defn cprime? [n]
  ((cand ((c<= (cn 2)) n))
    (cnot (cright
            ((((c- n) (cn 2))
              (fn [p] ((cpair (cinc (cleft p))) ((cor (cright p)) ((cdiv? (cinc (cleft p))) n)))))
             ((cpair (cn 1)) cfalse))))))

(def cnil
  cfalse)

(def cfirst
  cleft)

(def crest
  cright)

(defn cempty? [l]
  ((l (fn [h]
        (fn [t]
          (fn [d]
            cfalse))))
   ctrue))

(defn clist [& args]
  (reduce #((cpair %2) %1) cnil (map cn (reverse args))))

(defn cprintl [l]
  (if (cprintp (cempty? l))
      nil
      (conj (cprintl (crest l)) (cprint (cfirst l)))))

(def cmap
  (T (fn [m]
       (fn [f]
         (fn [l]
           (((cempty? l) cnil) (fn [x] (((cpair (f (cfirst l))) ((m f) (crest l))) x))))))))

(def cfoldr
  (T (fn [fld]
       (fn [f]
         (fn [v]
           (fn [l]
             (((cempty? l) v) (fn [x] (((f (cfirst l)) (((fld f) v) (crest l))) x)))))))))

(def cfilter
  (T (fn [f]
       (fn [p]
         (fn [l]
           (((cempty? l) cnil)
            (fn [x] ((((p (cfirst l))
                      ((cpair (cfirst l)) ((f p) (crest l))))
                      ((f p) (crest l))) x))))))))
