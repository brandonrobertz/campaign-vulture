;;;; (c) 2013 Bradon Robertz ... RIP EWOK VDB
;;;; GPLv3+ (I'm considering Snowtide PDFTextStream a system lib for now)
;; this needs to be set before pdftextstream is loaded at all
(System/setProperty "pdfts.layout.detectTables" "N")
(ns parse-tx-cfr.core
  (:gen-class)
  (:use [parse-tx-cfr similarity])
  (:require [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.tools.cli :as clitools])
  (:import [com.snowtide.pdf OutputTarget RegionOutputTarget
                             PDFTextStream Page PDFTextStreamConfig]
           [com.snowtide.pdf.layout BlockParent Block Line]))

;;; NOTE:
;;; Right now I'm using snowtide's PDFTextStream library. I will
;;; change to Apache PDFBox of possibly iText, because of the single-
;;; thread restrictions put on the *free* version of PDFTextStream and the
;;; license incompatabilities (PDFTextStream is nonredistributable).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  R E G I O N S  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn do-region
  "Adds a region with name s to our RegionOutputTarget object
   tgt. Specify x, y, w, and h."
  [^RegionOutputTarget tgt x y w h s]
  (.addRegion tgt x y w h s))

(defn do-regions
  "Take a list of key-value maps specifying regions to grab.
   i.e.:
   [{:name 'cool1' :x 100 :y 100 :w 100 :h 10}
      ...
    {:name 'coolN' :x 200 :y 100 :w 100 :h 10}]"
  [^RegionOutputTarget tgt m]
  (doseq [i m]
    (let [s (:name i)
          x (:x i)
          y (:y i)
          w (:w i)
          h (:h i)]
      (do-region tgt x y w h s))))

(defn region->string
  "When given an area to look, region->string will grab
   the information contained inside that area in a list
   of strings."
  [^Page pg x y w h]
  (let [^RegionOutputTarget tgt (RegionOutputTarget.)]
    (do-region tgt x y w h "lol")
    (.pipe pg tgt)
    (.getRegionText tgt "lol")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  P D F  B A S I C  U N I T S  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; All the disgusting java interop goes here ... waiting to be replaced
;;; by free code.

(defn blocks
  "When given a blockParent, returned by .getTextContent (usually from
   a  page), it will return a list of all of the blocks contained in
   that blockParent."
  [^BlockParent block-parent]
  (for [x (range (.getChildCnt block-parent))]
    (.getChild block-parent x)))

(defn lines
  "For a list of blocks, this function will return a list of lines for
   each block in the list."
  [blocks]
  (map #(for [x (range (.getLineCnt ^Block %))] (.getLine ^Block % x)) blocks))

(defn text-units
  "Takes a list of text units (possibly 2-dimensional, list of lists of
   text units) and returns a list of strings of the text within."
  [lines]
  (map
    (fn [line]
      (map
        #(for [x (range (.getTextUnitCnt ^Line %))] (.getTextUnit ^Line % x))
        line))
    lines))

(defn num-pages
  "Return the number of pages in a PDF document."
  [^PDFTextStream stream]
  (.getPageCnt stream))

(defn get-pg
  "Grab n page from a PDF document (a stream in this case)."
  [^PDFTextStream stream n]
  (.getPage stream n))

(defn get-stream
  "Get the PDF stream object."
  [^String filename]
  (PDFTextStream. filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  C O N V E R S I O N  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lines->infomap
  "Take a list of lists of lines and turn it into a list of
   maps containing rendered strings, and their properties (like
   position)."
  [lines]
  (map
    (fn [line]
      (map
        #(let [txt (StringBuilder. 1024)]
          (.pipe ^Line % (OutputTarget. txt))
          {:txt (str txt)
           :x (.xpos ^Line %)
           :y (.ypos ^Line %)})
        line))
    lines))

(defn page->infomap_OLD
  "Take a page and convert the structured information into lists of lists
   of strings, representing the heriarchy of the document's structure."
   [^Page pg]
   (->> (.getTextContent pg)
        (blocks)
        (lines)
        (lines->infomap)
        (flatten)))

(defn walk-blocks
  "Same as above, make sure that we recursivley pull out all lines from blocks"
  [blox]
  (flatten
   (for [i   (range (count blox))
         :let [blk (nth blox i)
               cnt (.getChildCnt blk)]]
     (if (= 0 cnt)
       blk
       (for [c (range cnt)]
         (.getChild blk c))))))

(defn page->infomap
  "Take a page and convert the structured information into lists of lists
   of strings, representing the heriarchy of the document's structure."
   [^Page pg]
   (->> (.getTextContent pg)
        (blocks)
        (walk-blocks)
        (lines)
        (lines->infomap)
        (flatten)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  F I N D I N G  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
(defn pnlz
  "Square the first item of a vector. Used to penalize."
  [c]
  (vector (first c) (* (second c) (second c))))

;;; by 2-dimensional distance
(defn dist
  "Euclidean distance for two collections, assumed x y pairs. I penalize
   y distance, making it more important."
  [c1 c2]
  (->> (map - (pnlz c1) (pnlz c2)) (map #(* % %)) (reduce +) (. Math sqrt)))

(defn closest
  "Get closest object (infomap) in PDF infomap by Euclidean distance."
  [c m]
  ;(do (println "c:" c) (println "m:" m) (flush)) ;DEBUG
  (apply min-key #(dist ((juxt :x :y) c) ((juxt :x :y) %)) (remove #(= c %) m)))

(defn header-contributor?
  "Check a string/infomap to see if it contains the contributor header."
  [m]
  (if (re-find #"name\sof\scontributor" (:txt m m)) true false))

;;; TODO
;;; Approximate or fuzzy matching instead of regex
;;; possibly construct a list of commonly error-prone characters
;;; and construct a search of these groups from a string ...
;;; or defmacro something to construct sets of regexes

;; Approximate (best) matching ;;
(defn str-in-map-fuzzy
  "Take a string and infomap/string and return their dice-similarity
  (for approximate matching)."
  [s m]
  (similarity s (:txt m m)))

(defn sim-map
  "Take a page and a desired string to match, and generate
   a list of infomaps of :txt, :x, :y, and :sim (similarity)."
  [pg s]
  (for [i (page->infomap pg)]
    (into i {:sim (str-in-map-fuzzy s i)})))

;; approximate
(defn find-by-str-fuzzy
  "Take a page and do a fuzzy search for the top n matches on the page."
  [pg s n]
  (take n (sort-by :sim > (sim-map pg s))))

;; EXACT (regex) matching ;;
(defn str-in-map?
  "Take a string/infomap and see if it matches the given string."
  [^String s m]
  ;; right now, we're just normalizing spaces and case
  (if (re-find (re-pattern (.toUpperCase s))
               (.toUpperCase (clojure.string/replace (:txt m m) #"\s+" " ")))
    true
    false))

;; EXACT (picks matches based on binary truth) MATCHING
(defn find-by-str
  "Take a string and a page, and return the infomap(s)
   containing that string, for every match found on the page."
  [pg s]
  (for [i (page->infomap pg)
        :when (str-in-map? s i)]
    i))

;;; NOTE
;;; I considered implementing width and height ... the problem is that for some
;;; PDF objects, their widths and heights are mostly empty space, which overlap
;;; with other objects. This makes it useless when you grab the text.
(defn delta
  "When given two strings and a page, find the correct infomaps and return
   a delta y and x, which is used to find one (s2) from the other (s1).
   Since situations can arrise where the strings will bring up many infomaps,
   (as in the case of a repeated header), use s1 as the string that can contain
   many matches and s2 as a unique string. We will pick the s1 closest to s2.

   NOTE: This will be used to generate a generic distance from a known header
   to a value that we want. We will query the page for headers, subtract
   a delta from the header's position to get the values we want."
  [pg s1 s2]
  (let [a (first (find-by-str pg s2)) ;unique
        b (closest a (find-by-str pg s1))]
    {:dy (- (:y b) (:y a)) :dx (:x a)}))

(defn delta-fuzzy
  "Generate delta values, using fuzzy string matching."
  [pg cfg s1 s2]
  (let [a (first (find-by-str-fuzzy pg s2 1)) ;unique
        b (closest a (find-by-str-fuzzy pg s1 (:recs-per-pg cfg)))]
    {:dy (- (:y b) (:y a)) :dx (:x a)}))

(defn batch-deltas
  "Take a page & a list of headers and example (training) values and return the
   appropriate delta values with their header strings.

   Headers & example vals (m) are in the following format:
   [[\"header1\" \"example value1\"]
    ...
    [\"headerN\" \"example valueN\"]]"
  [cfg pg]
  (for [x (:cfg cfg)
        :let [s1 (first  x)
              s2 (second x)
              n (:recs-per-pg cfg)]]
    ;NOTE just plugged in fuzzy matching here, can remove here 2
    (into {:txt s1} (delta-fuzzy pg cfg s1 s2))))

(defn delta-from-config
  "Returns a delta based on a specific config."
  [config]
  (println "Generating delta from config file" (:filename config))
  (let [stream     (get-stream (:filename config))
        example-pg (get-pg stream (:config-page config))
        deltas     (batch-deltas config example-pg)]
    deltas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  G E T  S T U F F  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These functions generally wrap the above, which are more raw tools for
;;; matching strings and pulling matches out of pages.

(defn contributor-headers
  "Take a list of strings or infomaps and return the contributor headers. For
   use with regions and the coords they provide. Assuming list is flat."
  [l]
  (for [i l
        :let [s (:txt i i)]
        :when (header-contributor? s)]
    i))

;;; TODO
;;; In the future there will have to be an auto-configuration
;;; of delta & width & height to achieve maximum righteousness
(defn contributor-names
  "Take a page, and return the names under the header, based
   on region estimation."
  [pg]
  (for [i (page->infomap pg)
        :let [x (:x i)
              y (:y i)
              ;make these configurable in the future
              delta 20 width 200 height 10]
        :when (header-contributor? i)]
    (region->string pg x (- y delta) width height)))

;;; This is the main search function that we will use to grab our values
;;; all that's needed is a header value of the value we want and a delta
;;; (the distance from the header to our value)
(defn vals-by-header
  "On a given page, find a header, indicated by a match
   of head-str, and then look delta units down (positive int,
   or neg for up) within an area specified by width (w) and
   height (h)."
  [pg head-str dy dx w h]
  (for [i (page->infomap pg)
        :let [y (:y i)]
        :when (str-in-map? head-str i)]
    (region->string pg dx (- y dy) w h)))

(defn vals-by-header-fuzzy
  "Same as above, but do it with fuzzy string matching."
  [pg cfg head-str dy dx w h]
  (for [i (sort-by :y > (find-by-str-fuzzy pg head-str (:recs-per-pg cfg)))
        :let [y (:y i)]]
    (region->string pg dx (- y dy) w h)))

(defn vals-from-deltamaps
  "Take a list of delta maps, resulting from a call to batch-deltas,
   and search a given page for the values pointed at by the header/delta
   pairs."
  [pg cfg m]
  (for [x m
        :let [txt (:txt x)
              dy  (:dy  x)
              dx  (:dx  x)
              w 250
              h 5]]
    (vals-by-header-fuzzy pg cfg txt dy dx w h)))

(defn restruct
  "Turn our one list per type of info to one-list per page, containing
   one vector per record."
  [m]
  (map #(apply vector (identity %))
       (partition (count m) (apply interleave m))))

(defn join-pages
  "Turn our one list w/ vectors per page to one long vector list."
  [l]
  (filter vector? (tree-seq seq? identity l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  RANGE FINDING  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn contributors?
  "Check a list to see if it contains phrases that indicate we're looking
   at a page that contains contribution information."
  [l]
  (and
   (> (count (filter #(re-find #"SCHEDULE" %)
                    (flatten (map #(:txt %) l)))) 0)
   (> (count (filter #(re-find #"OTHER\s+?THAN\s+?PLEDGES" %)
                     (flatten (map #(:txt %) l)))) 0)))

(defn contributors-pg-OLD?
  "Checks wether or not a page is a contributor page. Wrapper for
  contributors? function."
  [pg]
  (contributors? (page->infomap pg)))

(defn contributors-pg?
  [pg]
  (< 0.8
     (similarity "OTHER THAN PLEDGES OR LOANS"
                 (:txt (first (find-by-str-fuzzy
                               pg "OTHER THAN PLEDGES OR LOANS" 1))))))

(defn auto-range
  "Return a range of pages to scrape, based on calls to contributors-pg?"
  [stream]
  (println "Calculating auto-range")
  (let [n     (num-pages stream)
        r     (for [c (range (num-pages stream))
                    :when (contributors-pg? (get-pg stream c))] c)]
    (if (> (count r) 0)
      ;; calculate range based on min/max
      (let [min-r (first (sort < r))
            max-r (first (sort > r))]
        (range min-r (+ 1 max-r)))
      ;; No pages
      (range 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  C O N F I G U R E  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These functions will facilitate the configuration of what values to grab
;;; and allow the user to configure the program preferences easily.
;;; TODO: MAKE THESE! I'm thinking a swing (seesaw) interface that displays the
;;; PDF "DOM" and lets the user click on & select the lines to use as examples.

(defn config
  "This is a placeholder config. For use with data/test.pdf page 10"
  []
  {:filename   "data/2012CFRpts.pdf"
   :recs-per-pg 5
   :config-page 2
   :cfg [["Full name of contributor"    "Acuna, Gerard"]
         ["Contributor address"         "PO Box 26499"]
         ["Contributor address"         "Austin, TX 78755-0499"]
         ["Amount of contribution ($)"  "$350.00"]
         ["Employer (See Instructions)" "TRI Recycling Inc"]
         ["Principal occupation / Job title (See Instructions)"
          "President"]]})

(defn to-file
  "Save a clojure form to a file"
  [filename form]
  (with-open [w (java.io.FileWriter. filename)]
    (print-dup form w)))

(defn from-file
  "Load a clojure form from file."
  [filename]
  (println "Loading saved config" filename)
  (with-open [r (java.io.PushbackReader. (java.io.FileReader. filename))]
    (read r)))

;;;;  W R I T E  2  C S V  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clean-up
  [scraped-info amount-col-num]
  (map #(map-indexed (fn [i s]
                       (if (= i amount-col-num)
                         (string/upper-case
                          (string/replace
                           (string/trim-newline
                            (string/trim
                             (string/replace s #"\." "."))) #"[^0-9.]" ""))
                         (string/upper-case
                          (string/trim-newline
                           (string/trim
                            (string/replace s #"[^A-Za-z0-9 -,. ]" ""))))))
                     %)
       scraped-info))

(defn write-out
  [filename data]
  (with-open [out-file (io/writer filename)]
    (csv/write-csv out-file data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  M A I N  F U N C T I O N S  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn build-config-deltafile
  "Builds delta and saves it (with config), based on a given config"
  [config config-filename delta-filename]
  (do
    (println "Writing config to:" config-filename)
    (to-file config-filename config)
    (println "Writing delta to:" delta-filename)
    (to-file delta-filename (delta-from-config config))))

(defn scrape-page
  "Scrape data off page in PDF stream, specified by cfg."
  [pg cfg deltas]
  (restruct (vals-from-deltamaps pg cfg deltas)))

(defn scrape-pages
  [stream start end]
  (let [cfg        (config)
        example-pg (get-pg stream (:config-page cfg))
        deltas     (batch-deltas cfg example-pg)]
    (join-pages
     (for [n (range start end)
           ;; n (range (num-pages stream))
           ;; n (10 11) ;test case WORKS error-free
           ;; [n (range (num-pages stream))]
           :let [nil1 (println "Extracting contributors on page" n)
                 pg (get-pg stream n)]]
       (scrape-page pg cfg deltas)))))

(defn scrape-pages-wrapper
  "Scrape a document, looking at each to dermine if it's a contributor page.
   Uses given config and delta structs."
  [stream config delta range]
  (join-pages
   (for [n range ;(range (num-pages stream))
         :let [pg (get-pg stream n)]]
     (do
       ;; DEBUG OUTPUT
       (println "Extracting contributors on page" n)
       (scrape-page pg config delta)))))

; for convenience in coding ... due to multiproc restrictions
; w/ snowtide, it's easier to use a global instance
(defn global-stream  "Load a global stream instance." []
  (try
    (def stream (get-stream "data/2012CFRpts.pdf"))
    (catch Exception e (println "stream already defined"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  CLI  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parse-args
  "Parse out args and return the resulting [options arguments summary]"
  [args]
  (clitools/cli
   args
   ["-m"  "--mode"
      (apply str "parse-tx-cfr has two modes:\n\t\t\t\t"
             " 'parse' and 'config'. Parse mode is the\n\t\t\t\t"
             " main mode, but it depends on having\n\t\t\t\t"
             " config files built from config mode.")]
   ["-c"  "--config"        "Save/load a pre-built config file"]
   ["-d"  "--delta"         "Save/load a pre-build delta file"]
   ["-r"  "--auto-range"    (apply str "Try to automatically find\n\t\t\t\t"
                                   "range of contributor information\n\t\t\t\t"
                                   "Note: This may fail without warning.")
    :parse-fn #(boolean %)
    :default false]
   ["-h"  "--help"]))

(defn usage [options-summary]
  (->> ["Extract contributor information from a PDF campaign finance report"
        ""
        "Usage: parse-tx-cfr [options] infile outfile start end"
        ""
        "Options:"
        options-summary
        ""
        "Parse mode arguments:"
        "  infile   Input PDF to extract contributor information from"
        "  outfile  Output CSV to write contributor information to"
        "  start    Page to start extracting on (zero indexed)"
        "  end      Last page to extract information from (zero indexed)"
        ""]
              (string/join \newline)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main
  [& args]
  (let [[options arguments summary] (parse-args args)]
    (println "Selected options:")
    (println "  mode:      " (:mode options false))
    (println "  config:    " (:config options false))
    (println "  delta:     " (:delta options false))
    (println "  auto-range:" (:auto-range options false))
    (println (count arguments) "Arguments:\n" arguments)
    ;; HELP / USAGE
    (if (:help options false) (exit 0 (usage summary)))
    ;; MODE (make sure we have a mode and config and delta)
    (if (and (:mode   options false)
             (:config options false)
             (:delta  options false))
      (cond
       ;; PARSE MODE (make sure we have 4 args)
       (= (:mode options) "parse")
       ;; execute scrape-pages (with sloppy debug output)
       (let [debug1  (println "Entering parse mode")
             infile  (nth arguments 0)
             outfile (nth arguments 1)
             stream  (get-stream infile)
             config  (from-file  (:config options))
             delta   (from-file  (:delta  options))
             start   (if (= (count arguments) 4)
                       (Integer/parseInt (nth arguments 2)))
             end     (if (= (count arguments) 4)
                       (Integer/parseInt (nth arguments 3)))
             debug2  (println "Parsing pages" start "through" end
                              "of" infile "to" outfile)]
         ;; MANUAL vs AUTO-RANGE MODE based on auto-range opt & num of args
         (if (and (false? (:auto-range options false))
                  (= 4 (count arguments)))
           ;; Manual range
           (do
             (println "Manual range mode")
             (write-out outfile
                        (clean-up
                         (scrape-pages-wrapper stream
                                               config
                                               delta
                                               (range start end)) 3)))
           ;; Auto range
           (do
             (println "Auto-range mode")
             (write-out outfile
                        (clean-up
                         (scrape-pages-wrapper stream
                                               config
                                               delta
                                               (auto-range stream)) 3)))))
       ;; CONFIG-BUILD MODE
       (= (:mode options) "config")
       (let [nil1 (println "Entering config mode")]
         (build-config-deltafile
          (config)
          (:config options)
          (:delta options)))
       :else (do (println "options:" options)
                 (println "arguments:" arguments)
                 (println (usage summary))
                 (exit 1 (usage summary))))))
  (println "Done! Exiting."))

;;;; LEE LEFFINGWELL DEMONSTRATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn demo []
  (write-out "/home/uzr/Desktop/leff-scraped.csv"
             (concat (clean-up (scrape-pages stream   2  56) 3)
                     (clean-up (scrape-pages stream  70 181) 3)
                     (clean-up (scrape-pages stream 211 255) 3)
                     (clean-up (scrape-pages stream 278 296) 3))))
