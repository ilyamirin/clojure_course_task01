(ns task01.core
  (:require [pl.danieljanus.tagsoup :refer :all])
  (:gen-class))

(defn link? [element]
  (= (:class (get element 1)) "r"))

(defn get-all-descendants [roots]
  (mapcat #(filter vector? %) roots))

(defn parse-a-alements [data]
  (get-all-descendants 
        (loop [children (filter vector? data) result []] 
          (if (empty? children)
            result
            (recur 
              (get-all-descendants children)
              (into result (filter link? children)))))))

(defn get-links []
  (let [data (parse "clojure_google.html")]
    (map #(:href (get % 1)) (parse-a-alements data))))

(defn -main []
  (println (str "Found " (count (get-links)) " links!")))


