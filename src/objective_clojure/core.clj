(ns objective-clojure.core)

;;(def testfile "/Users/humasect/hv/experiments/objective-clojure-gamelike-actor.lisp")
(def testfile "gamelike-actor.oclj")

(defn read-forms [stream]
  (when-let [form (read stream false nil false)]
    (cons form (read-forms stream))))

(defn load-forms [filename]
  (with-in-str (slurp filename) (read-forms *in*)))
