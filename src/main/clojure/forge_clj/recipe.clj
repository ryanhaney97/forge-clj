(ns forge-clj.recipe
  "Contains functions for handling crafting and smelting recipes."
  (:require
   [forge-clj.util :refer [itemstack]]
   [clojure.string :as string])
  (:import
   [net.minecraft.item ItemStack]
   [net.minecraftforge.fml.common.registry GameRegistry]))

(defn convert-recipe
  "Converts a shaped recipe specified in the addrecipe function,
  into an object array to be passed to the registry."
  [recipe]
  (let [layout (:layout recipe)
        layout (string/replace layout #"_" " ")
        layout (string/split layout #"\n")
        layout (mapv string/trim layout)
        bindings (:bindings recipe)
        bindings (into [] bindings)
        bindings (flatten bindings)]
    (object-array (concat layout bindings))))

(defn convert-shapeless-recipe
  "Converts a shapeless recipe specified in the addrecipe function,
  into an object array to be passed to the registry."
  [recipe]
  (let [items (:items recipe)
        per-item (fn [imap]
                   (itemstack (:item imap) (if (:quantity imap) (:quantity imap) 1) (if (:metadata imap) (:metadata imap) 0)))
        items (mapv per-item items)]
    (object-array items)))

(defn addrecipe
  "Given a resulting item or block, as well as a map containing the details of the recipe,
  will register a recipe for the specified item or block in the GameRegistry.

  The methodology used to determine the recipe is based on whether or not it is shapeless,
  which is provided via the :shapeless keyword.

  If the recipe is shaped, expects the following keywords:

  :layout - contains a string that uses characters as recipe positions, with empty space or an _ representing nothing.
  :bindings - map that binds characters used in :layout with the respective itemstack used in the recipe.

  Otherwise, if the recipe, is shapeless, expects a :item keyword bound to an vector of itemstack data or maps representing the properties of an itemstack."
  [result recipe]
  (let [shapeless? (:shapeless recipe)
        quantity (if (:quantity recipe) (:quantity recipe) 1)
        metadata (if (:metadata recipe) (:metadata recipe) 0)
        result-itemstack (itemstack result quantity metadata)
        recipe-array (if shapeless? (convert-shapeless-recipe recipe) (convert-recipe recipe))]
    (if shapeless?
      (GameRegistry/addShapelessRecipe result-itemstack recipe-array)
      (GameRegistry/addRecipe result-itemstack recipe-array))))

(defn addsmelting
  "Given an input item or block, an output item or block, and the amount of experience gained from the smelt,
  registers a smelting recipe in the GameRegistry."
  [input output exp]
  (let [^ItemStack input (if (instance? ItemStack input)
                           input
                           (itemstack input))
        ^ItemStack output (if (instance? ItemStack output)
                            output
                            (itemstack output))]
    (GameRegistry/addSmelting input output (float exp))))
