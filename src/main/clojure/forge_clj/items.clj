(ns forge-clj.items
  "Contains macros for creating Items and specific types of Items."
  (:require
   [forge-clj.core :refer [defobj]])
  (:import
   [java.util Random]
   [net.minecraft.block Block BlockContainer]
   [net.minecraft.block.material Material]
   [net.minecraft.item Item ItemArmor ItemFood ItemSword ItemPickaxe ItemAxe ItemSpade ItemHoe]
   [net.minecraftforge.common.util EnumHelper]))

(defmacro defitem
  "DEFOBJ: Creates an anonymous instance of an Item with the specified properties."
  [item-name & args]
  (let [itemdata (apply hash-map args)]
    `(defobj Item [] ~item-name ~itemdata)))

(defmacro deftoolmaterial
  "MACRO: Creates a tool material."
  [material-name harvest-level durability mining-speed damage enchantability]
  `(def ~material-name (EnumHelper/addToolMaterial ~(str material-name) ~harvest-level ~durability ~(float mining-speed) ~(float damage) ~enchantability)))

(defmacro deftool
  "DEFOBJ: Creates an anonymous instance of a tool.
  Additionally requires a toolmaterial and a keyword representing the tool's type (or a class representing an Item's tool that takes a material in its constructor) to be passed in after the name."
  [item-name material tooltype & args]
  (let [itemdata (apply hash-map args)
        tool (condp = tooltype
               :sword `ItemSword
               :pickaxe `ItemAxe
               :axe `ItemAxe
               :spade `ItemSpade
               :shovel `ItemSpade
               :hoe `ItemHoe
               tooltype)]
    `(defobj ~tool [~material] ~item-name ~itemdata)))

(defmacro defarmormaterial
  "MACRO: Creates an armor material. The third argument for damage-reduction can be either a map or a vector (if vector it goes in the order of helmet, chest, legs, boots)."
  [material-name durability damage-reduction enchantability]
  (let [damage-reduction (if (map? damage-reduction) [(:helmet damage-reduction)
                                                      (:chestplate damage-reduction)
                                                      (:leggings damage-reduction)
                                                      (:boots damage-reduction)] damage-reduction)]
    `(def ~material-name (EnumHelper/addArmorMaterial ~(str material-name) ~durability (int-array ~damage-reduction) ~enchantability))))

;Given the respective arguments, creates a piece of armor. Requires a previously defined armor material,
;as well as the piece of armor.
(defmacro defarmor
  "DEFOBJ: Creates an anonymous instance of an ItemArmor.
  Additionally requires an armormaterial and a keyword representing the armor's type (or the respective number) to be passed in after the name.

  The following keyword is treated specially:

  :renderindex - specifies the render index passed to the constructor of the armor. Defaults to 0."
  [item-name material armortype & args]
  (let [itemdata (apply hash-map args)
        renderindex (if (:renderindex itemdata) (:renderindex itemdata) 0)
        itemdata (dissoc itemdata :renderindex)
        armor (condp = armortype
                :helmet 0
                :chestplate 1
                :leggings 2
                :boots 3
                armortype)]
    `(defobj ItemArmor [~material ~renderindex ~armor] ~item-name ~itemdata)))

(defmacro deffood
  "DEFOBJ: Creates an anonymous instance of an ItemFood.

  The following keyword is treated specially:

  :wolves-favorite? - specifies if this is a wolves favorite, that is passed to the constructor. Defaults to false."
  [item-name heal-amount saturation-modifier & args]
  (let [itemdata (apply hash-map args)
        wolves-favorite? (some? (:wolves-favorite? itemdata))
        itemdata (dissoc itemdata :wolves-favorite?)]
    `(defobj ItemFood [~heal-amount ~(float saturation-modifier) ~wolves-favorite?] ~item-name ~itemdata)))
