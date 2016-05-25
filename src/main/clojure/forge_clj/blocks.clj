(ns forge-clj.blocks
  "Contains macros related to creating blocks."
  (:require
    [forge-clj.core :refer [defobj genobj]])
  (:import
    [net.minecraft.block Block BlockContainer]
    [net.minecraft.block.properties PropertyHelper IProperty]
    [net.minecraft.block.material Material]
    [net.minecraft.block.state BlockState IBlockState]
    [net.minecraft.item ItemBlock]
    [com.google.common.collect ImmutableSet]))

(defn keywords->property [name keywords]
  (genobj PropertyHelper
          [name clojure.lang.Keyword]
          {:override {:get-name (fn
                                  ([]
                                   (let [^PropertyHelper this this]
                                     (proxy-super getName)))
                                  ([p]
                                   (if (instance? clojure.lang.Named p)
                                     (name p)
                                     (str p))))
                      :get-allowed-values (fn []
                                            (ImmutableSet/of (into-array clojure.lang.Keyword keywords)))}}))

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

(defn state->map [map-keys ^IBlockState state]
  (into {} (map #(vector %1 (.getValue state (name %1))) map-keys)))

(defn add-state-to-blockdata [blockdata state]
  (let [properties (map->properties state)
        meta-map (make-meta-map state)
        blockdata (if (get-in blockdata [:override :create-block-state])
                    blockdata
                    (assoc-in blockdata [:override :create-block-state] `(fn []
                                                                           (BlockState. ~'this (into-array IProperty ~(vals properties))))))
        meta->property (map #(list '.withProperty `(.getDefaultState ~'this) (name (key %1)) (get meta-map 'meta)) properties)
        blockdata (if (get-in blockdata [:override :get-state-from-meta])
                    blockdata
                    (assoc-in blockdata [:override :get-state-from-meta] `(fn [~'meta]
                                                                            ~(cons `do meta->property))))
        blockdata (if (get-in blockdata [:override :get-meta-from-state])
                    blockdata
                    (assoc-in blockdata [:override :get-meta-from-state] `(fn [~(with-meta 'state {:tag `IBlockState})]
                                                                            (get ~(clojure.set/map-invert meta-map) (state->map ~(keys properties) ~'state)))))]
    blockdata))

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
                    blockdata)]
    `(defobj ~(if container? `BlockContainer `Block) [~material] ~block-name ~blockdata)))

(defmacro defitemblock
  "DEFOBJ: Creates an anonymous instance of an ItemBlock with the specified properties."
  [item-name block & args]
  (let [itemdata (apply hash-map args)]
    `(defobj `ItemBlock [~block] ~item-name ~itemdata)))

