(ns forge-clj.blocks
  (:require
   [forge-clj.core :refer [defobj]])
  (:import
   [net.minecraft.block Block BlockContainer]
   [net.minecraft.block.material Material]
   [net.minecraft.item ItemBlock ItemBlockWithMetadata]))

;Given the name of a block, and a series of keywords and values representing the properties of the block,
;generates a Block object with the specified properties. Methods can be overriden using the :override keyword.
(defmacro defblock [block-name & args]
  (let [blockdata (apply hash-map args)
        material (if (:material blockdata) (:material blockdata) `Material/rock)
        container? (:container? blockdata)
        blockdata (dissoc blockdata :material :container?)]
    `(defobj ~(if container? `BlockContainer `Block) [~material] ~block-name ~blockdata)))

;Given a previously defined block, creates an item for that block (aka an ItemBlock).
;Register this together with the block referenced.
(defmacro defblockitem [item-name block & args]
  (let [itemdata (apply hash-map args)
        meta? (:metadata? itemdata)
        itemdata (dissoc itemdata :metadata?)]
    `(defobj ~(if meta? `ItemBlockWithMetadata `ItemBlock) ~(if meta? [block block] [block]) ~item-name ~itemdata)))
