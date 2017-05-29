(ns app.design-parse
  (:require [instaparse.core :as insta]))

; --- parses older ctgov xml; as of 2017/1/11 the design is now more structured 
(def design
  (insta/parser
    "design = keyval (<sep> keyval)*
     sep = ', '
     keyval = key <colon> val
     key = #'[\\w -]*\\w'
     colon = ': '
     val = valstr spec?
     <spec> = <' ('> valstr (<sep> valstr)* <')'>
     <valstr> = #'[\\w/ -]*\\w'"))
(defn parse [the-str]
  (if (= "N/A" the-str)
    [:design]
    (design the-str)))
(defn design-as-map [the-str]
  (into {} (map (fn [x] [(second (second x)) (rest (nth x 2))]) (rest (parse the-str)))))

; extracts blinding information from the ctgov masking string
(def masking
  (insta/parser
    "masking = blindingtype <space> specs
     blindingtype = #'[\\w -]*\\w'
     space = ' '
     specs = <paropen> val(<sep> val)* <parclose>
     paropen = '('
     parclose = ')'
     sep = ', '
     val = #'[\\w/ -]*\\w'"))
(defn parse-masking [the-str]
  (if (= "N/A" the-str)
    "N/A"
    (let 
      [masking-map (masking the-str)
       blinding (second (second masking-map))
       extra-terms (map #(second %1) (rest (nth masking-map 2)))]
      (concat (list blinding) extra-terms))))

