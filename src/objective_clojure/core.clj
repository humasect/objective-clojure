(ns objective-clojure.core)

(def testfile "gamelike-actor.oclj")

;;;;;;;;;;;;;

(defn third [x]
  (first (rest (rest x))))

(defn fourth [x]
  (first (rest (rest (rest x)))))

;;;;;;;;;;;;;

(defn read-forms [stream]
  (when-let [form (read stream false nil false)]
    (cons form (read-forms stream))))

(defn load-forms [filename]
  (with-in-str (slurp filename) (read-forms *in*)))

;;;;;;;;;;;;;;;;;;;;

(defn comma-sep
  "[1 2 3] => '1, 2, 3'"
  [lst]
  (join (interpose ", " lst)))

(defn c-ify [x]
  "[:name int] => int name
   [:*window ClassName] => ClassName *window"
  (str (second x) " " (name (first x))))

(defn c-decls
  "{:name int :*test char} => {int name; char *test;}"
  [decls]
  (join (interpose "; " (map #'c-ify decls))))

(defn c-args
  "{:name int :*test char} => (int name, char *test)"
  [args]
  (comma-sep (map #'c-ify args)))

(defn objc-args
  "{:initWith:arg:} => initWith: arg:"
  [args]
  (str args))

(defn objc-for-form [form]
  (case (name (first form))
    "import" (str "#import " (second form) "\n")
    "class" (str "@class " (comma-sep (second form)) ";\n")
    "interface" (str "@interface " (second form) " : " (third form)
                     "\n{\n" (c-decls (fourth form)) "\n}\n")
    "property" (str "@property (" (comma-sep (second form)) ") " (c-decls (third form)))
    ;; "+" (str "+ (" (c-type (second form)) ") " (objc-decl (third form)))
    ;; "-" (str "- (" (c-type (second form)) ") " (objc-decl (third form)))))
    "end" (str "@end\n")
    ))

;;;;;;;;;;;;;;;;;;;;;;;

(defn objc-test []
  (map #'objc-for-form (load-forms testfile)))

(defn lib
  ([x] (when x (str (first x) ", " (lib (rest x))))))

(defn lib2 [x]
  (map #(str %1 ", ") x))