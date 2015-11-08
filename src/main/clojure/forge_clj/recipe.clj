(ns forge-clj.recipe
  (:require
   [forge-clj.util :refer [itemstack]]
   [clojure.string :as string])
  (:import
   [net.minecraft.item ItemStack]
   [cpw.mods.fml.common.registry GameRegistry]))

;Converts a shaped recipe specified in the addrecipe function, into an object array to be passed to the registry.
(defn convert-recipe [recipe]
  (let [layout (:layout recipe)
        layout (string/replace layout #"_" " ")
        layout (string/split layout #"\n")
        layout (mapv string/trim layout)
        bindings (:bindings recipe)
        bindings (into [] bindings)
        bindings (flatten bindings)]
    (object-array (concat layout bindings))))

;Converts a shapeless recipe specified in the addrecipe function, into an object array to be passed to the registry.
(defn convert-shapeless-recipe [recipe]
  (let [items (:items recipe)
        per-item (fn [imap]
                   (itemstack (:item imap) (if (:quantity imap) (:quantity imap) 1) (if (:metadata imap) (:metadata imap) 0)))
        items (mapv per-item items)]
    (object-array items)))

;Given a resulting item or block, as well as a map containing the details of the recipe,
;will register a recipe for the specified item or block. The methodology used to determine the recipe
;is based on whether or not it is shapeless, which is provided via the :shapeless keyword.
(defn addrecipe [result recipe]
  (let [shapeless? (:shapeless recipe)
        quantity (if (:quantity recipe) (:quantity recipe) 1)
        metadata (if (:metadata recipe) (:metadata recipe) 0)
        result-itemstack (itemstack result quantity metadata)
        recipe-array (if shapeless? (convert-shapeless-recipe recipe) (convert-recipe recipe))]
    (if shapeless?
      (GameRegistry/addShapelessRecipe result-itemstack recipe-array)
      (GameRegistry/addRecipe result-itemstack recipe-array))))

;Given an input item or block, an output item or block, and the amount of experience gained from the smelt,
;registers a smelting recipe in the GameRegistry.
(defn addsmelting [input output exp]
  (let [^ItemStack input (if (instance? ItemStack input)
                           input
                           (itemstack input))
        ^ItemStack output (if (instance? ItemStack output)
                           output
                           (itemstack output))]
    (GameRegistry/addSmelting input output (float exp))))
