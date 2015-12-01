(ns forge-clj.blocks
  "Contains macros related to creating blocks."
  (:require
   [forge-clj.core :refer [defobj]])
  (:import
   [net.minecraft.block Block BlockContainer]
   [net.minecraft.block.material Material]
   [net.minecraft.item ItemBlock ItemBlockWithMetadata]))

(defmacro defblock
  "DEFOBJ: Generates an anonymous instance of a Block with the specified properties.

  The following keywords are treated specially:

  :material - specifies a Material to use for the block, defaults to Material/rock.
  :container? - setting this to true will make this a BlockContainer instead of a Block, allowing the use of Tile Entities with it."
  [block-name & args]
  (let [blockdata (apply hash-map args)
        material (if (:material blockdata) (:material blockdata) `Material/rock)
        container? (:container? blockdata)
        blockdata (dissoc blockdata :material :container?)]
    `(defobj ~(if container? `BlockContainer `Block) [~material] ~block-name ~blockdata)))

;Given a previously defined block, creates an item for that block (aka an ItemBlock).
;Register this together with the block referenced.
(defmacro defblockitem
  "DEFOBJ: Creates an anonymous instance of an ItemBlock with the specified properties.

  The following keywords are treated specially:

  :metadata? - if set to true, will make this an ItemBlockWithMetadata, allowing metadata to be used with it."
  [item-name block & args]
  (let [itemdata (apply hash-map args)
        meta? (:metadata? itemdata)
        itemdata (dissoc itemdata :metadata?)]
    `(defobj ~(if meta? `ItemBlockWithMetadata `ItemBlock) ~(if meta? [block block] [block]) ~item-name ~itemdata)))
