(ns forge-clj.items
  "Contains macros for creating Items and specific types of Items."
  (:require
    [forge-clj.core :refer [defobj]]
    [forge-clj.util :refer [defmemo]])
  (:import
    [net.minecraft.creativetab CreativeTabs]
    [net.minecraft.item Item ItemArmor ItemFood ItemSword ItemPickaxe ItemAxe ItemSpade ItemHoe]
    [net.minecraftforge.common.util EnumHelper]))

(defmacro deftab
  "DEFOBJ: Creates an anonymous instance of CreativeTabs."
  [tab-name & args]
  (let [obj-data (apply hash-map args)]
    `(defobj CreativeTabs [~(str tab-name)] ~tab-name ~obj-data)))

(defmacro defitem
  "DEFOBJ: Creates an anonymous instance of an Item with the specified properties."
  [item-name & args]
  (let [itemdata (apply hash-map args)
        itemdata (if (:unlocalized-name itemdata)
                   itemdata
                   (assoc itemdata :unlocalized-name (str item-name)))]
    `(defobj Item [] ~item-name ~itemdata)))

(defmemo gen-tool-material
         [material-name harvest-level durability mining-speed damage enchantability]
         (EnumHelper/addToolMaterial (str material-name) harvest-level durability (float mining-speed) (float damage) enchantability))

(defmacro deftoolmaterial
  "MACRO: Creates a tool material."
  [material-name harvest-level durability mining-speed damage enchantability]
  `(def ~material-name (gen-tool-material ~material-name ~harvest-level ~durability ~mining-speed ~damage ~enchantability)))

(defmacro deftool
  "DEFOBJ: Creates an anonymous instance of a tool.
  Additionally requires a toolmaterial and a keyword representing the tool's type (or a class representing an Item's tool that takes a material in its constructor) to be passed in after the name."
  [item-name tooltype material & args]
  (let [itemdata (apply hash-map args)
        material (if (or (symbol? material) (list? material))
                   (if (map? (eval material))
                     (eval material)
                     material)
                   material)
        material (if (map? material)
                   `(gen-tool-material
                      ~(:name material (str item-name "-material"))
                      ~(:harvest-level material 0)
                      ~(:durability material 1)
                      ~(:mining-speed material 1)
                      ~(:damage material 0)
                      ~(:enchantability material 5))
                   material)
        tool (condp = tooltype
               :sword `ItemSword
               :pickaxe `ItemPickaxe
               :axe `ItemAxe
               :spade `ItemSpade
               :shovel `ItemSpade
               :hoe `ItemHoe
               tooltype)
        itemdata (if (:unlocalized-name itemdata)
                   itemdata
                   (assoc itemdata :unlocalized-name (str item-name)))]
    `(defobj ~tool [~material] ~item-name ~itemdata)))

(defmemo gen-armor-material
         [material-name texture-name durability damage-reduction enchantability]
         (let [damage-reduction (if (map? damage-reduction) [(:helmet damage-reduction)
                                                             (:chestplate damage-reduction)
                                                             (:leggings damage-reduction)
                                                             (:boots damage-reduction)] damage-reduction)]
           (EnumHelper/addArmorMaterial (str material-name) (str texture-name) durability (int-array damage-reduction) enchantability)))

(defmacro defarmormaterial
  "MACRO: Creates an armor material. The fourth argument for damage-reduction can be either a map or a vector (if vector it goes in the order of helmet, chest, legs, boots)."
  [material-name texture-name durability damage-reduction enchantability]
  `(def ~material-name (gen-armor-material ~material-name ~texture-name ~durability ~damage-reduction ~enchantability)))

;Given the respective arguments, creates a piece of armor. Requires a previously defined armor material,
;as well as the piece of armor.
(defmacro defarmor
  "DEFOBJ: Creates an anonymous instance of an ItemArmor.
  Additionally requires an armormaterial and a keyword representing the armor's type (or the respective number) to be passed in after the name.

  The following keyword is treated specially:

  :renderindex - specifies the render index passed to the constructor of the armor. Defaults to 0."
  [item-name armortype material & args]
  (let [itemdata (apply hash-map args)
        material (if (or (symbol? material) (list? material))
                   (if (map? (eval material))
                     (eval material)
                     material)
                   material)
        material (if (map? material)
                   `(gen-armor-material
                      ~(:name material (str item-name "-material"))
                      ~(:texture-name material)
                      ~(:durability material 1)
                      ~(:damage-reduction material [2 6 5 2])
                      ~(:enchantability material 5))
                   material)
        renderindex (if (:renderindex itemdata) (:renderindex itemdata) 0)
        itemdata (dissoc itemdata :renderindex)
        itemdata (if (:unlocalized-name itemdata)
                   itemdata
                   (assoc itemdata :unlocalized-name (str item-name)))
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
        itemdata (dissoc itemdata :wolves-favorite?)
        itemdata (if (:unlocalized-name itemdata)
                   itemdata
                   (assoc itemdata :unlocalized-name (str item-name)))]
    `(defobj ItemFood [~heal-amount ~(float saturation-modifier) ~wolves-favorite?] ~item-name ~itemdata)))
