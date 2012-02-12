(ns objective-clojure.core
  (:use [clojure.string :only [join]]
        [clojure.java.io :only [file]]
        [clojure.string :only [split upper-case]]))

;;;;;;;;;;;;;

(defn third [x]
  (first (rest (rest x))))

(defn fourth [x]
  (first (rest (rest (rest x)))))

(defmacro path-concat [& cs]
  `(.getPath (file ~@cs)))

;; (defmacro join-map [&x]
;;   `(join (map ~@x)))

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

;;;;;;;;;;;;;;;;;;;;;;;;

(defn objc-args
  "{:newWithDictionary dict :andObject obj} => newWithDictionary: dict andObject: obj"
  [x]
  (if (= (class x) clojure.lang.PersistentList)
    (join (map #(str " " (name (key %1)) ": " (val %1)) x))
    x))

(defn objc-method
  [form]
  (str (first form) " (" (second form) ")" (objc-args (third form))
       (if (fourth form)
         (c-block (fourth form))
         ";\n")))

(defmulti objc-import )
(defn objc-import [form]
  )

(defn objc-for-form [form]
  (case (name (first form))
    "import" (str "#import " (second form) "\n")
    "class" (str "@class " (comma-sep (rest form)) ";\n")
    "interface" (str "@interface " (second form) " : " (third form)
                     "\n{\n" (c-decls (fourth form)) "\n}\n")
    "implementation" (str "@implementation " (second form) "\n\n")
    "property" (str "@property (" (comma-sep (rest (rest form))) ") " (c-args (second form)) ";\n")
    "dynamic" (str "@dynamic " (comma-sep (rest form)) ";\n")
    "synthesize" (str "@synthesize " (comma-sep (rest form)) ";\n")
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

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compile-objc
  ([inf outf]
     (print "\t" inf " -> " outf "\n")
     (spit outf (join (parse-objc (slurp inf)))))
  ([in inext out outext]
     (doseq [inf (files-in-dir-of-ext in inext)]
       (let [outf (str (remove-file-ext inf) "." outext)] 
         (compile-objc inf outf)))))

(defn compile-dir-to-dir [in out]
  (compile-objc in "cljh" out "h")
  (compile-objc in "cljm" out "m"))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-project [dir]
  (let [fdir (path-concat dir "xcode.clj")]
    (with-in-str (slurp fdir) (read))))

(defn project-item-to-makefile-item [i]
  (let [k (name (key i))]
    (case (first k)
      \- (val i)
      (str (upper-case k) " = " (val i)))))

(defn project-to-makefile [prj]
  (join (interpose "\n\n" (map #'project-item-to-makefile-item prj))))

(defn xcodebuild-str [mode]
  (join [mode ;;(join (take (-> mode count dec) (rest mode)))
    ":\n\txcodebuild -target $(TARGET) -configuration $(CONFIG) -sdk $(SDK) " mode]))

(defn get-home-dir []
  (System/getenv "HOME"))

(defn slurp-project-spit-makefile [dir]
  (spit (path-concat dir "Makefile")
        (->
         (read-project dir)
         (assoc ,,
     ;;:codesign_allocate "/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/codesign_allocate"
             :build-dir "build"
             :-all "all: build\n\t@"
             :-build (xcodebuild-str "build")
             :-clean (xcodebuild-str "clean")
             :-dist (join ["xcrun -sdk $(SDK) PackageApplication -v $(BUILD-DIR)/$(PRODUCT).app"
                           " -o $(BUILD-DIR)/$(PRODUCT).ipa --sign $(SIGN)"
                           " --embed '" (path-concat (get-home-dir) "Library/MobileDevice/")
                           "Provisioning Profiles/$(PROFILE).mobileprovision'"]))
         project-to-makefile)))
