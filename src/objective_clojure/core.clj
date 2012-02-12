(ns objective-clojure.core
  (:use [clojure.string :only [join]]
        [clojure.java.io :only [file]]
        [clojure.string :only [split]]))

;;;;;;;;;;;;;

(defn third [x]
  (first (rest (rest x))))

(defn fourth [x]
  (first (rest (rest (rest x)))))

(defmacro path-concat [& cs]
  `(.getPath (file ~@cs)))

(defn remove-file-ext [file-name]
  ;; http://formpluslogic.blogspot.com/2009/08/clojure-unit-testing-part-1.html
  (let [index (.lastIndexOf file-name ".")]
    (apply str (first (split-at index file-name)))))

(defn file-extension [x]
  (last (-> x (split #"\."))))

(defn files-in-dir [dir]
  (map #(path-concat dir (.getName %1))
       (-> dir file .listFiles)))

(defn files-in-dir-of-ext [dir ext]
  (filter #(= (file-extension %1) ext)
          (files-in-dir dir)))

;;;;;;;;;;;;;

(defn comma-sep
  "[1 2 3] => '1, 2, 3'"
  [lst]
  (join (interpose ", " lst)))

(defn colon-sep
  "{:newWithDictionary dict :andObject obj} => newWithDictionary: dict andObject: obj"
  [x]
  (if (= (class x) clojure.lang.PersistentList)
    (join (map #(str " " (name (key %1)) ": " (val %1)) x))
    x))

(defn c-ify
  "[:name int] => int name
   [:*window ClassName] => ClassName *window"
  [x]
  (str (second x) " " (name (first x))))

(defn c-decls
  "{:name int :*test char} => int name; char *test;"
  [decls]
  (str (join (interpose "; " (map #'c-ify decls))) ";"))

(defn c-args
  "{:name int :*test char} => int name, char *test"
  [args]
  (comma-sep (map #'c-ify args)))

(defn objc-args
  "{:initWith:arg:} => initWith: arg:"
  [args]
  (str args))

(defn c-for-form [form]
  (let [c (join (map #'str (rest form)))]
    (case (name (first form))
      "return" (str "\treturn " c ";\n")
      "do" (c-block (second form)))))

(defn c-bracify [form]
  (str "{\n" form "}\n"))

(defmulti c-block class)
(defmethod c-block clojure.lang.PersistentVector [form]
  (c-bracify (join (map #'c-for-form form))))
(defmethod c-block :default [form]
  (c-bracify (c-for-form form)))

(defn objc-method
  [form]
  (str (first form) " (" (second form) ")" (colon-sep (third form))
       (if (fourth form)
         (c-block (fourth form)))
       ";\n"))

(defn objc-for-form [form]
  (case (name (first form))
    "import" (str "#import " (second form) "\n")
    "class" (str "@class " (comma-sep (rest form)) ";\n")
    "interface" (str "@interface " (second form) " : " (third form)
                     "\n{\n" (c-decls (fourth form)) "\n}\n")
    "implementation" (str "@implementation " (second form) "\n\n")
    "property" (str "@property (" (comma-sep (rest (rest form))) ") " (c-args (second form)) ";\n")
    "dynamic" (str "@dynamic " (comma-sep (rest form)) ";")
    "synthesize" (str "@synthesize " (comma-sep (rest form)) ";")
    "+" (objc-method form)
    "-" (objc-method form)
    "end" (str "@end\n")
    ))

;;;;;;;;;;;;;;;;;;;;;;;

(defn read-forms [stream]
  (when-let [form (read stream false nil false)]
    (cons form (read-forms stream))))

(defn parse-objc [string]
  (with-in-str string (map #'objc-for-form (read-forms *in*))))

(defn compile-objc [in out]
  (spit out (join (parse-objc (slurp in)))))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dest-files [])

(defn compile-dir [in inext out outext]
  (doseq [inf (files-in-dir-of-ext in inext)]
    (let [outf (str (remove-file-ext inf) "." outext)]
      (print inf " -> " outf "\n"))))

(defn compile-dir-to-dir [in out]
  (compile-dir in "cljh" out "h")
  (compile-dir in "cljm" out "m"))
