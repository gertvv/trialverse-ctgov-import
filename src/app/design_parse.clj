(ns app.design-parse
  (:require [instaparse.core :as insta]))

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
  (design the-str))

(defn design-as-map [the-str]
  (into {} (map (fn [x] [(second (second x)) (rest (nth x 2))]) (rest (parse the-str)))))
