(ns pddl-clojure-interface.core
  (:require [clojure.tools.reader.edn :as edn]
            [clojure.java.io :as io]))


(defn read-sexp
  "Read one s-expression from a file"
  [filename]
  (with-open [rdr (java.io.PushbackReader.
                   (clojure.java.io/reader filename))]
    (edn/read rdr)))

(defmacro write->file
  "Write body to the given file name"
  [filename & body]
  `(do
     (with-open [w# (io/writer ~filename)]
     (binding [*out* w#]
       ~@body))))

(defn get-PDDL-construct
  "Get a PDDL code block."
  [pddl-block pddl-file]
  (filter #(and (seq? %)
                (= (name pddl-block)
                   (name (first %))))
          (read-sexp pddl-file)))


(defn get-PDDL-predicates
  "Get all predicates in a PDDL domain"
  [pddl-file]
  (first (get-PDDL-construct 'predicates pddl-file)))

(defn get-PDDL-init
  "Get init code block in a PDDL file"
  [pddl-file]
  (first (get-PDDL-construct 'init pddl-file)))

(defn get-PDDL-types
  "Get all types in a PDDL file"
  [pddl-file]
  (first (get-PDDL-construct 'types pddl-file)))

(defn add-part-to-PDDL
  "Add the specified part to the
specified position of a PDDL file"
  [pddl-file position part]
  
  (map #(if (and (seq? %)
                 (= (keyword position) (first %)))
          (concat % part)
          %)
       (read-sexp pddl-file)))

(defn create-PDDL-types
  "Add type to a list of entities"
  [ls type]
  (map #(str % " - " type)
       ls))

(defn read-entities-and-add-type
  "Read PDDL objects from a file and add type
  (e.g. 'table bed' -> (list table - furniture
                        bed - furniture))"
  [file type]
  (create-PDDL-types (clojure.string/split (slurp file) #"\s")
                     type))

(defn find-new-file-name
  "Take a filename and determines, the new number
that has to be added to create a new file. E.g.
file1.img file2.img file3.img means that, file4.img
has to be created"
  [filename extension]
  (loop [n 0]
    (if-not (io/.exists (io/as-file
                         (str filename n extension)))
      (str filename n extension)
      (recur (inc n)))))

(defn get-types-in-predicate
  "Takes a PDDL predicate,
  e.g. '(at ?x - location ?y - object)
  and returns the involved types, e.g.
  '(location object)"
  [pddl-pred]
  (remove
   (fn [s]
     (let [first-char (first (name s))]
       (or (= \- first-char)
           (= \? first-char)))) (rest pddl-pred)))
