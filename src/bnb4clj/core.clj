(ns bnb4clj.core)


(defmacro unless [test & body]
  `(when-not ~test ~@body))

(defmacro unwind-protect [form & body]
  `(try ~form
        (finally ~@body)))

(defmacro ignore-errors [& forms]
  `(try (do ~@forms) (catch Exception e nil)))

(defmacro until [test & body]
  `(while (not ~test) ~@body))

(defmacro for-ever [& body]
  `(while true ~@body))

(defn file-size [file]
  (.length (if (string? file)
             (java.io.File. file)
             file)))

(defmacro show
  ([variable-name]
   `(dprint ~(str variable-name) "=" ~variable-name))
  ([name & more]
   `(do ~@(map (fn [name] `(show ~name)) (cons name more)))))

(defn map-plist [f plist]
  (map f (take-nth 2 plist) (take-nth 2 (rest plist))))

(defn mapcat-plist [f plist]
  (mapcat f (take-nth 2 plist) (take-nth 2 (rest plist))))

(defn reduce-plist
  "Reduce a property list. F must be a function that accepts three
  arguments: the accumulating result of the reduction, the key, and
  the value associated to the key."
  ([f init plist]
   (reduce (fn [result [k v]]
             (f result k v))
           init (map-plist vector plist)))
  ([f plist]
   (reduce (fn [result [k v]]
             (f result k v))
           (map-plist vector plist))))

(defn plist->map
  "Convert a property list to a map."
  [plist]
  (apply hash-map plist)
  ;; use hash-map instead
  #_(reduce-plist (fn [result k v] (assoc result k v)) {} plist))

(defn readable? [obj]
  (instance? java.lang.Readable obj))

(defn file? [obj]
  (instance? java.io.File obj))

(defmacro tcase [obj & cases]
  "Like Common Lisp TYPE-CASE."
  `(condp instance? ~obj
     ~@cases))

(defmacro with-gensyms [symbols & forms]
  `(let [~@(mapcat (fn [sym]
                     [sym (list 'gensym (str sym))])
                   symbols)]
     ~@forms))

(defn map->properties
  "Convert  a Clojure map to a Java Properties object.
  (There is a number of Java functions that require such object as
  input.)"
  [map]
  (let [props (java.util.Properties.)]
    (doseq [[key value] map]
      (.put props
            (if (keyword? key)
              (name key)
              (str key))
            (str value)))
    props))

(defmacro condp2
  "Just like CONDP but the arguments passed to PRED are swapped."
  [pred obj & clauses]
  (with-gensyms [object predicate]
    `(let [~object ~obj
           ~predicate ~pred]
       (cond
         ~@(mapcat (fn [el form]
                     (list (list predicate object el)
                           form))
                   (take-nth 2 clauses)
                   (take-nth 2 (rest clauses)))))))

(defn partition-with
  "Applies F to each value in SEQUENCE, splitting it each time F
  returns true.  Returns a lazy seq of partitions, that do not include
  the elements for which F is true.

  Given F=odd? and SEQUENCE=[1 2 4 6 7 8 9 10 12 16] this function returns:
  ([] [2 4 6] [8] [10 12 16])"
  [f sequence]
  (letfn [(make [s]
            (lazy-seq
             (when (seq s)
               (loop [in s
                      out []]
                 (cond
                   (empty? in) (cons out (make in))
                   (f (first in)) (cons out (make (rest in)))
                   :else (recur (rest in) (conj out (first in))))))))]
    (make sequence)))
