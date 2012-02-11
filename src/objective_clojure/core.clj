(ns objective-clojure.core)

(def testfile "/Users/humasect/hv/experiments/objective-clojure-gamelike-actor.lisp")

(defn load-forms [s])

(defn test []
  (with-in-str (slurp filename)
    (let [eof nil]
      (while (not eof) ))))

(defn test2 []
  (with-in-str (slurp testfile)
    (loop []
      (when (not (read nil nil))))))

(defn load-db [filename]
  (with-in-str (slurp filename) (read)))

;;;;;

(defn read-forms [stream]
  (when-let [form (read stream false nil false)]
    (cons form (read-forms stream))))

(defn load-forms [filename]
  (with-in-str (slurp filename) (read-forms *in*)))
