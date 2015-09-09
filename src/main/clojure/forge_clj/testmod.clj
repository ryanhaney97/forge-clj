(ns forge-clj.testmod
  (:require [forge-clj.core :refer :all]))

(defitem testitem
  :creative-tab (creative-tab :misc))

(defblock testblock
  :override {:get-item-dropped (fn [_ _ _] testitem)}
  :hardness 0.5
  :step-sound (step-sound :stone)
  :creative-tab (creative-tab :block)
  :light-level (float 1.0))

(defn init [this event]
  (register testblock "testblock")
  (register testitem "testitem"))

(defmod forge-clj.testmod test-mod 0.0.1 init)
