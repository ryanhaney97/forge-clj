(ns forge-clj.items
  (:require
   [forge-clj.core :refer [defobj]])
  (:import
   [java.util Random]
   [net.minecraft.block Block BlockContainer]
   [net.minecraft.block.material Material]
   [net.minecraft.item Item ItemArmor ItemFood ItemSword ItemPickaxe ItemAxe ItemSpade ItemHoe]
   [net.minecraftforge.common.util EnumHelper]))

;Given the name of an item, and a series of keywords and values representing the properties of the item,
;generates an Item object with the specified properties. Methods can be overriden using the :override keyword.
(defmacro defitem [item-name & args]
  (let [itemdata (apply hash-map args)]
    `(defobj Item [] ~item-name ~itemdata)))

;Given the respective arguments, creates a tool material.
(defmacro deftoolmaterial [material-name harvest-level durability mining-speed damage enchantability]
  `(def ~material-name (EnumHelper/addToolMaterial ~(str material-name) ~harvest-level ~durability ~(float mining-speed) ~(float damage) ~enchantability)))

;Given the resepctive arguments, creates a tool. Requires a previously defined tool material,
;as well as the type of the tool.
(defmacro deftool [item-name material tooltype & args]
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

;Given the respective arguments, creates an armor material.
(defmacro defarmormaterial [material-name durability damage-reduction enchantability]
  (let [damage-reduction (if (map? damage-reduction) [(:helmet damage-reduction)
                                                      (:chestplate damage-reduction)
                                                      (:leggings damage-reduction)
                                                      (:boots damage-reduction)] damage-reduction)]
    `(def ~material-name (EnumHelper/addArmorMaterial ~(str material-name) ~durability (int-array ~damage-reduction) ~enchantability))))

;Given the respective arguments, creates a piece of armor. Requires a previously defined armor material,
;as well as the piece of armor.
(defmacro defarmor [item-name material armortype & args]
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

;Given the respective arguments, creates food.
(defmacro deffood [item-name heal-amount saturation-modifier & args]
  (let [itemdata (apply hash-map args)
        wolves-favorite? (some? (:wolves-favorite? itemdata))
        itemdata (dissoc itemdata :wolves-favorite?)]
    `(defobj ItemFood [~heal-amount ~(float saturation-modifier) ~wolves-favorite?] ~item-name ~itemdata)))
