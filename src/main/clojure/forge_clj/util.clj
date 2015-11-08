(ns forge-clj.util
  (:require
   [clojure.string :as string]
   [clojure.set :as cset]
   [forge-clj.core :refer [defobj]])
  (:import
   [java.util Random]
   [net.minecraft.block Block]
   [net.minecraft.item Item ItemStack]
   [net.minecraft.creativetab CreativeTabs]
   [net.minecraft.world World]
   [cpw.mods.fml.common.registry GameRegistry]))

(defmulti make-itemstack (fn [item amount metadata] (type item)))
(defmethod make-itemstack Item [^Item item amount metadata]
  (ItemStack. item (int amount) (int metadata)))
(defmethod make-itemstack Block [^Block block amount metadata]
  (ItemStack. block (int amount) (int metadata)))

;Makes an ItemStack. For convenience.
(defn itemstack
  ([item]
   (itemstack item 1))
  ([item amount]
   (itemstack item amount 0))
  ([item amount metadata]
   (make-itemstack item amount metadata)))

;Convenient .isRemote check function. Looks cleaner.
(defn remote? [^World world]
  (.isRemote world))

;Utility function. Given a map and a function, applies that function to all values in the map.
(defn update-map-vals [func m]
  (into {} (map #(vector (key %1) (func (val %1))) m)))

;Utility function. Given a map and a function, applies that function to all keys in the map.
(defn update-map-keys [func m]
  (cset/map-invert (update-map-vals func (cset/map-invert m))))

;Extremely basic function that returns the absolute value of a number. For convenience.
(defn abs [n]
  (if (< n 0)
    (* -1 n)
    n))

(defmacro defmemo [memo-name arg-vector & args]
  `(def ~memo-name
     (memoize
      (fn ~arg-vector
        ~@args))))

(defmacro deftab [tab-name & args]
  (let [obj-data (apply hash-map args)]
    `(defobj CreativeTabs [~(str tab-name)] ~tab-name ~obj-data)))

(defn get-item
  ([name-and-id]
   (let [split-id (string/split name-and-id #":")]
     (get-item (first split-id) (second split-id))))
  ([modid item-name]
   (GameRegistry/findItem (str modid) (str item-name))))

(defn get-block
  ([name-and-id]
   (let [split-id (string/split name-and-id #":")]
     (get-block (first split-id) (second split-id))))
  ([modid block-name]
   (GameRegistry/findBlock (str modid) (str block-name))))
