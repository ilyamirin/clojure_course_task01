(ns task01.core
  (:require [pl.danieljanus.tagsoup :refer :all])
  (:gen-class))

(defn link? [element]
  (if (= (:class (get element 1)) "r")
    true
    false))

(defn get-all-descendants [roots]
  (reduce #(into %1 (filter vector? %2)) [] roots))

(defn get-links []
" 1) Find all elements containing {:class \"r\"}.

Example:
[:h3 {:class \"r\"} [:a {:shape \"rect\", :class \"l\",
                         :href \"https://github.com/clojure/clojure\",
                         :onmousedown \"return rwt(this,'','','','4','AFQjCNFlSngH8Q4cB8TMqb710dD6ZkDSJg','','0CFYQFjAD','','',event)\"}
                     [:em {} \"clojure\"] \"/\" [:em {} \"clojure\"] \" · GitHub\"]]

   2) Extract href from the element :a.

The link from the example above is 'https://github.com/clojure/clojure'.

  3) Return vector of all 10 links.

Example: ['https://github.com/clojure/clojure', 'http://clojure.com/', . . .]
"
  (let [data (parse "clojure_google.html")]
    (reduce 
      #(conj %1 (:href (get %2 1)))
      []
      (get-all-descendants 
        (loop [children (filter vector? data) result []] 
          (if (empty? children)
            result
            (recur 
              (get-all-descendants children)
              (into result (filter link? children)))))))))

(defn -main []
  (println (str "Found " (count (get-links)) " links!")))


