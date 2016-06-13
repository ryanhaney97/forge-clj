(ns forge-clj.blocks
  "Contains macros related to creating blocks."
  (:require
    [forge-clj.core :refer [defobj defclass]]
    [forge-clj.util :refer [with-prefix]])
  (:import
    [net.minecraft.block Block BlockContainer]
    [net.minecraft.block.properties IProperty PropertyHelper]
    [net.minecraft.block.material Material]
    [net.minecraft.block.state BlockState IBlockState]
    [net.minecraft.item ItemBlock ItemStack]
    [com.google.common.collect ImmutableSet ImmutableMap]))

(defclass net.minecraft.block.properties.PropertyHelper key-property
          {:init "init"
           :state "keywords"
           :expose {getName superGetName}
           :constructors {[String clojure.lang.PersistentVector] [String Class]}})

(with-prefix
  key-property-
  (defn init
    [property-name keywords]
    [[property-name clojure.lang.Keyword] keywords])
  (defn getName
    ([^KeyProperty this]
     (.superGetName this))
    ([_ property]
     (name property)))
  (defn getAllowedValues
    [^KeyProperty this]
    (.-keywords this)))

(defn keywords->property [name keywords]
  (KeyProperty. name keywords))

(defn map->properties [property-map]
  (into {} (map #(vector (key %1) (keywords->property (name (key %1)) (val %1))) property-map)))

(defn num-properties [property-map]
  (apply * (map count (vals property-map))))

(defn get-val-for-meta-at-property-number [property-map meta-value property-num]
  (let [key-name (nth (keys property-map) property-num)
        data (get property-map key-name)
        divisor (if (= property-num 0)
                  1
                  (apply * (map count (take property-num (vals property-map)))))
        result-index (mod (int (/ meta-value divisor)) (count data))]
    (get data result-index)))

(defn make-meta-map
  ([property-map]
   (make-meta-map property-map 0 {}))
  ([property-map meta-value result]
   (if (not (< meta-value (num-properties property-map)))
     result
     (recur property-map (inc meta-value) (assoc result meta-value (zipmap (keys property-map)
                                                                           (mapv (partial get-val-for-meta-at-property-number property-map meta-value) (range (count (keys property-map))))))))))

(defn state->map [properties ^IBlockState state]
  (into {} (map #(vector (keyword (.getName ^IProperty %1)) (keyword (.getValue state %1))) properties)))

(defn add-state-to-blockdata [blockdata state]
  (let [meta-map (make-meta-map state)
        blockdata (assoc blockdata :class true)
        blockdata (if (get-in blockdata [:override :create-block-state])
                    blockdata
                    (assoc-in blockdata [:override :create-block-state] `(fn []
                                                                           (BlockState. ~'this (into-array IProperty (vals (map->properties ~state)))))))
        meta->property (map #(list '.withProperty `(keywords->property ~(name (key %1)) ~(val %1)) `(get (get ~meta-map ~'meta) ~(key %1))) state)
        blockdata (if (get-in blockdata [:override :get-state-from-meta])
                    blockdata
                    (assoc-in blockdata [:override :get-state-from-meta] `(fn [~'meta]
                                                                            ~(concat `(-> (.getDefaultState ~(with-meta 'this {:tag `Block}))) meta->property))))
        blockdata (if (get-in blockdata [:override :get-meta-from-state])
                    blockdata
                    (assoc-in blockdata [:override :get-meta-from-state] `(fn [~(with-meta 'block-state {:tag `IBlockState})]
                                                                            (get ~(clojure.set/map-invert meta-map) (state->map (vals (map->properties ~state)) ~'block-state)))))
        default-property-setters (map #(list '.withProperty `(keywords->property ~(name (key %1)) ~(val %1)) (first (val %1))) state)
        blockdata (if (get blockdata :default-state)
                    blockdata
                    (assoc blockdata :default-state (concat `(doto (.getBaseState (.getBlockState ~(with-meta 'this {:tag `Block})))) default-property-setters)))]
    blockdata))

(defn guava-map->clojure-map [^ImmutableMap gmap]
  (let [g-keys (into [] (.toArray (.keySet gmap)))
        g-vals (map #(.get gmap %1) g-keys)]
    (zipmap g-keys g-vals)))

(defn get-state
  ([^Block block ^ItemStack item]
   (let [^IBlockState state (.getStateFromMeta block (.getMetadata item))
         ^ImmutableMap state-map (.getProperties state)
         state-map (guava-map->clojure-map state-map)]
     (into {} (mapv #(vector (keyword (.getName ^IProperty (key %1))) (keyword (val %1))) state-map)))))

(defmacro defblock
  "DEFOBJ: Generates an anonymous instance of a Block with the specified properties.

  The following keywords are treated specially:

  :material - specifies a Material to use for the block, defaults to Material/rock.
  :container? - setting this to true will make this a BlockContainer instead of a Block, allowing the use of Tile Entities with it."
  [block-name & args]
  (let [blockdata (apply hash-map args)
        material (get blockdata :material `Material/rock)
        container? (:container? blockdata)
        state (:state blockdata)
        blockdata (dissoc blockdata :material :container? :state)
        blockdata (if state
                    (add-state-to-blockdata blockdata state)
                    blockdata)
        blockdata (if (:unlocalized-name blockdata)
                   blockdata
                   (assoc blockdata :unlocalized-name (str block-name)))]
    `(defobj ~(if container? `BlockContainer `Block) [~material] ~block-name ~blockdata)))

(defmacro defitemblock
  "DEFOBJ: Creates an anonymous instance of an ItemBlock with the specified properties."
  [item-name block & args]
  (let [itemdata (apply hash-map args)]
    `(defobj ItemBlock [~block] ~item-name ~itemdata)))
