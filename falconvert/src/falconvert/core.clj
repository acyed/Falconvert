(ns falconvert.core
  (:use clojess.core)
  (:use clojure.xml)
  (:require [clojure.zip :as zip])  
  (:gen-class))

(def db (atom (open-db "/Users/youngac/Downloads/16_nttr_overlay.drw")))

(def the-rows (atom (rows (table @db "Main"))))

(defn data-type-convert [number-in]
  (case number-in 
    1 :DATA_TYPE_TYPE
    3 :DATA_TYPE_MOVETO
    4 :DATA_TYPE_LINETO
    5 :DATA_TYPE_CENTER
    6 :DATA_TYPE_TEXT
    8 :DATA_TYPE_FONT
    9 :DATA_TYPE_TOOLTIP
    10 :DATA_TYPE_HELP
    11 :DATA_TYPE_COMMENT
    12 :DATA_TYPE_COLOR
    13 :DATA_TYPE_TEXT_PARAM
    14 :DATA_TYPE_LINK_NAME
    15 :DATA_TYPE_LINE_PARAM
    16 :DATA_TYPE_FIX_TEXT
    17 :DATA_TYPE_FIX_BEARING
    18 :DATA_TYPE_FONT_2
    19 :DATA_TYPE_TEXT_PARAM_2
    20 :DATA_TYPE_COLOR_2
    21 :DATA_TYPE_COLOR_3
    22 :DATA_TYPE_LINK_ARRAY_NAME
    23 :DATA_TYPE_LABEL_PARAM
    24 :DATA_TYPE_LABEL_PARAM
    25 :DATA_TYPE_LABEL_OFFSET
    26 :DATA_TYPE_LABEL_FONT_NAME
    27 :DATA_TYPE_GROUP
    28 :DATA_TYPE_PICTURE_OFFSET
    29 :DATA_TYPE_PICTURE_SCREENXY
    30 :DATA_TYPE_PICTURE_DISP_NAME
    31 :DATA_TYPE_PICTURE_SRC_NAME
    32 :DATA_TYPE_LL_GEO
    33 :DATA_TYPE_UR_GEO
    34 :DATA_TYPE_PICTURE_SIZE
    35 :DATA_TYPE_PICTURE_MAP_SOURCE
    36 :DATA_TYPE_PICTURE_MAP_SERIES
    37 :DATA_TYPE_PICTURE_MAP_SCALE
    38 :DATA_TYPE_PICTURE_MAP_ZOOM
    39 :DATA_TYPE_PICTURE_IMG_OFFSET))

(defn data-sub-type-convert [data-in]
  "Takes in an atom of row data and replaces the ints representing the datatype"
  (let [map-datatype #(assoc % :DataType (data-type-convert (:DataType %))) 
        new-data (doall (map map-datatype (vec @data-in)))]
    (reset! data-in new-data )))

;(data-sub-type-convert the-rows)

(defn convert-to-groups [an-atom-of-db-rows]
  (reset! an-atom-of-db-rows (group-by :ItemNum @an-atom-of-db-rows)))

;; (convert-to-groups the-rows)

(defn get-lat-longs-from-lines [row-data]
  (let [is-lineto-or-moveto? #(or (= (:DataType %) :DATA_TYPE_LINETO)
                                  (= (:DataType %) :DATA_TYPE_MOVETO))]
    (if (is-lineto-or-moveto? row-data) (:Data row-data))))

(defn get-lat-longs-vec [row-data]
  (into [] (filter some? (map get-lat-longs-from-lines row-data)))) 

(defn convert-line-data-type
  "takes the first element of a data element and returns the line object info"
  [the-line]
  (let [first-map (first the-line)] 
    (if (and  (= "01" (subs (:Data first-map) 0 2)) (= :DATA_TYPE_TYPE (:DataType first-map)))
      (let [data-string (:Data first-map)
            is-polygon? (subs data-string 8 9)]
        {:data-type (subs data-string 0 2)
         :line-width (subs data-string 2 4)
         :fill-style (subs data-string 4 5)
         :line-type (subs data-string 5 6)
         :line-text-type (subs data-string 6 7)
         :filler (subs data-string 7 8)
         :is-poloygon is-polygon?
         :has-label (subs data-string 9 10)
         :line-text-center-type (subs data-string 10 11)
         :lat-longs (let [current-vecs (get-lat-longs-vec the-line)]
                      (if (= is-polygon? "1")
                        (conj current-vecs (first current-vecs))
                        current-vecs) )})
      nil)))

;; (def line-types-only (atom (filter some? (map convert-line-data-type (vals @the-rows)))))



(defn convert-latlong-to-kml-format 
  "takes a lat long from the FV format and returns a string with long,lat"
  [lat-long-string]
  (let [northing (if (= "N" (subs lat-long-string 0 1))
                   ""
                   "-")
        northing-digits (subs lat-long-string 1 10)
        easting (if (= "E" (subs lat-long-string 10 11))
                  ""
                  "-")
        easting-digits (subs lat-long-string 11)]
    (str  easting easting-digits "," northing northing-digits)))

(defn join-all-lat-longs [lat-long-vec] 
  (clojure.string/join  (interleave  (map convert-latlong-to-kml-format lat-long-vec) (repeat  "\n")))) 

(defn change-all-to-strings [the-atom-of-lat-long-objects] 
  (reset! the-atom-of-lat-long-objects 
          (map #(assoc % :lat-longs (-> % :lat-longs join-all-lat-longs)) @the-atom-of-lat-long-objects)))

 ;; (change-all-to-strings line-types-only)

;;; XML working stuff
;; (require '[clojure.data.zip :as cdz])
;; (require '[clojure.data.zip.xml :as cdzx])

(def xml-file (atom (clojure.xml/parse "/Users/youngac/Documents/Clojure/fv2kmz/resources/test.kml")))

(defn a-polygon-node  [the-xml] (-> the-xml zip/xml-zip zip/next zip/next zip/rightmost zip/down zip/right  zip/down  zip/node))

(defn change-lat-longs-in-polygon [polygon lat-long-string]
  (-> polygon zip/xml-zip zip/next zip/next zip/next zip/next zip/next zip/down zip/down zip/down (zip/replace lat-long-string) zip/root))

(defn insert-polygon-node [the-xml new-node] 
  (let [the-new-xml (-> @the-xml zip/xml-zip zip/next zip/next zip/rightmost zip/down zip/right (zip/insert-child new-node) zip/root)]
    (reset! the-xml the-new-xml)))



;(map #(->> % :lat-longs (change-lat-longs-in-polygon a-polygon-node)) @line-types-only)

(defn replace-lat-longs [the-xml lat-long-string] (-> the-xml 
                                      zip/xml-zip 
                                      zip/next 
                                      zip/next 
                                      zip/rightmost 
                                      zip/next 
                                      zip/next 
                                      zip/next 
                                      zip/down 
                                      zip/next 
                                      zip/next 
                                      zip/next 
                                      zip/next 
                                      zip/down 
                                      zip/down 
                                      zip/down 
                                      (zip/replace lat-long-string)  
                                      zip/root))




(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def db (atom (open-db "/Users/youngac/Downloads/16_nttr_overlay.drw")))

  (def the-rows (atom (rows (table @db "Main"))))
  
  (data-sub-type-convert the-rows)
  
  (convert-to-groups the-rows)

  (def line-types-only (atom (filter some? (map convert-line-data-type (vals @the-rows)))))

  (change-all-to-strings line-types-only)
  
  (let [all-lat-longs (doall (into [] (map :lat-longs @line-types-only)))
      the-polygon (a-polygon-node @xml-file)
        new-nodes (doall (into [] (map #(change-lat-longs-in-polygon the-polygon %) all-lat-longs)))]
    (doall (for [i new-nodes] ( insert-polygon-node xml-file i)))
   (spit "output.kml" (with-out-str (clojure.xml/emit @xml-file)))))
