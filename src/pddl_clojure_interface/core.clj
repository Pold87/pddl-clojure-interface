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

(defn get-PDDL-construct-in-list
  "Get a PDDL code block in a sexp"
  [pddl-block ls]
  (filter #(and (seq? %)
                (= (name pddl-block)
                   (name (first %))))
          ls))


(defn get-PDDL-construct
  "Get a PDDL code block in a file"
  [pddl-block pddl-file]
  (get-PDDL-construct-in-list pddl-block
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

(defn add-part-to-PDDL-list
  "Add the specified part to the
specified position of a PDDL desription"
  [ls position part]
  (map #(if (and (seq? %)
                 (= (keyword position) (first %)))
          (concat % part)
          %)
       ls))

(defn add-part-to-PDDL
  "Add the specified part to the
specified position of a PDDL file"
  [pddl-file position part]
  (add-part-to-PDDL-list (read-sexp pddl-file)
                         position
                         part))

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
  "Take a filename and a extension and determine the revision number
that has to be added to create that file.
Example: If 'file1.png' and 'file2.png' exist return file3.png "
  [filename extension]
  (loop [n 0]
    (if-not (io/.exists (io/as-file
                         (str filename n extension)))
      (str filename n extension)
      (recur (inc n)))))

(defn get-types-in-predicate
  "Take a PDDL predicate,
  e.g. '(at ?x - location ?y - object)
  and return a list of the involved types, e.g.
  '(location object)"
  [pddl-pred]
  (remove
   (fn [s]
     (let [first-char (first (name s))]
       (or (= \- first-char)
           (= \? first-char)))) (rest pddl-pred)))
