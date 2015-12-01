(ns forge-clj.ui
  "Contains macros and methods involving uis and guis."
  (:require
   [clojure.string :as string]
   [forge-clj.core :refer [gen-classname]])
  (:import
   [cpw.mods.fml.common.network IGuiHandler NetworkRegistry]
   [net.minecraft.inventory Container Slot IInventory]
   [net.minecraft.entity.player EntityPlayer]
   [net.minecraft.item ItemStack]
   [java.util List]))

(defmacro defguihandler
  "MACRO: Creates an anonymouse instance of an IGuiHandler with the specified name, server function, and client function."
  [handler-name server-fn client-fn]
  `(def ~handler-name (reify IGuiHandler
                        (~'getServerGuiElement [~'this ~'id ~'player ~'world ~'x ~'y ~'z]
                                               (~server-fn ~'id ~'player ~'world ~'x ~'y ~'z))
                        (~'getClientGuiElement [~'this ~'id ~'player ~'world ~'x ~'y ~'z]
                                               (~client-fn ~'id ~'player ~'world ~'x ~'y ~'z)))))

(defn register-gui-handler
  "Registers a gui handler for the specified mod instance."
  [mod-instance handler]
  (.registerGuiHandler ^NetworkRegistry (NetworkRegistry/INSTANCE) mod-instance handler))

(defn add-slot-to-container
  "Given a container, an inventory, a slot number, and a slot x and y, adds a Slot to the provided Container."
  [^Container container ^IInventory inventory slot-number slot-x slot-y]
  (let [slot (Slot. inventory (int slot-number) (int slot-x) (int slot-y))]
    (set! (.-slotNumber slot) (.size ^List (.-inventorySlots container)))
    (.add ^List (.-inventorySlots container) slot)
    (.add ^List (.-inventoryItemStacks container) nil)
    slot))

(defn add-slots
  "Given a container, inventory and a vector containing vectors in the form of [slot-number slot-x slot-y],
  adds all of the slots in the vector to the container."
  [container inventory slots]
  (let [add-slot-to-this (partial add-slot-to-container container inventory)]
    (loop [slots slots]
      (if (not (first slots))
        true
        (let [slot (first slots)]
          (add-slot-to-this (first slot) (second slot) (nth slot 2))
          (recur (rest slots)))))))

(defn add-player-hotbar
  "Given a container and the player's inventory,
  adds the player's hotbar slots in their normal place to the container."
  [container player-inventory]
  (let [hotbar-slots (mapv #(vector %1 (+ (* %1 18) 8) 142) (range 9))]
    (add-slots container player-inventory hotbar-slots)))

(defn add-player-inventory
  "Given a container and the player's inventory,
  adds the player's inventory slots in their normal place to the container."
  [container player-inventory]
  (let [make-slot (fn [x y]
                    [(+ x (* y 9) 9) (+ 8 (* x 18)) (+ 84 (* y 18))])
        slots (into [] (for [x (range 9)
                             y (range 3)]
                         (make-slot x y)))]
    (add-slots container player-inventory slots)))

(defmacro defcontainer
  "MACRO: Given a namespace, a name, and some classdata, creates a Container class.
  The constructor for this class takes an instance of the player's inventory, and an instance of the bound inventory respectively.

  The following keywords are treated specially:

  :player-hotbar? - if true, adds the player's hotbar slots to the container.
  :player-inventory? - if true, adds the player's inventory slots to the container.
  :slots - if provided, adds the specified slots to the container. Should be a vector of vectors that contain data along the lines of [slot-number slot-x slot-y]
  :interfaces - vector of additional interfaces to implement
  :expose-methods - map of super methods to expose.
  :expose-fields - map of fields to expose. Useful for protected fields."
  [name-ns class-name & args]
  (let [classdata (apply hash-map args)
        hotbar? (:player-hotbar? classdata)
        inventory? (:player-inventory? classdata)
        slots (:slots classdata)
        prefix (str class-name "-")
        interfaces (get classdata :interfaces [])
        super-methods (:expose-methods classdata)
        exposed-fields (:expose-fields classdata)
        fullname (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname class-name)))
        this-sym (with-meta 'this {:tag fullname})]
    `(do
       (gen-class
        :name ~fullname
        :prefix ~prefix
        :extends Container
        :exposes-methods ~super-methods
        :init ~'initialize
        :post-init ~'post-initialize
        :constructors {[IInventory IInventory] []}
        :state ~'data
        :implements ~interfaces)
       (def ~class-name ~fullname)
       (import ~fullname)
       (defn ~(symbol (str prefix "initialize"))
         ([]
          [[] {}])
         ([~'player-inventory ~'bound-inventory]
          [[] {:player-inventory ~'player-inventory
               :bound-inventory ~'bound-inventory}]))
       (defn ~(symbol (str prefix "post-initialize"))
         ([~this-sym]
          nil)
         ([~this-sym ~'player-inventory ~'bound-inventory]
          ~(if slots
             `(add-slots ~this-sym ~'bound-inventory ~slots))
          ~(if inventory?
             `(add-player-inventory ~this-sym ~'player-inventory))
          ~(if hotbar?
             `(add-player-hotbar ~this-sym ~'player-inventory)))))))

(defn open-gui
  "Given a player, an instance of the mod, a gui's id, the current world, x, y, and z, attempts to open a gui."
  [^EntityPlayer player mod-instance id world x y z]
  (.openGui player mod-instance id world x y z))