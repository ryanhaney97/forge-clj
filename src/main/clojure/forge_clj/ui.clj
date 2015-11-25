(ns forge-clj.ui
  (:require
   [clojure.string :as string]
   [forge-clj.core :refer [gen-classname]])
  (:import
   [cpw.mods.fml.common.network IGuiHandler NetworkRegistry]
   [net.minecraft.inventory Container Slot IInventory]
   [net.minecraft.entity.player EntityPlayer]
   [net.minecraft.item ItemStack]
   [java.util List]))

(defmacro defguihandler [handler-name server-fn client-fn]
  `(def ~handler-name (reify IGuiHandler
                        (~'getServerGuiElement [~'this ~'id ~'player ~'world ~'x ~'y ~'z]
                                               (~server-fn ~'id ~'player ~'world ~'x ~'y ~'z))
                        (~'getClientGuiElement [~'this ~'id ~'player ~'world ~'x ~'y ~'z]
                                               (~client-fn ~'id ~'player ~'world ~'x ~'y ~'z)))))

(defn register-gui-handler [mod-instance handler]
  (.registerGuiHandler ^NetworkRegistry (NetworkRegistry/INSTANCE) mod-instance handler))

(defn add-slot-to-container [^Container container ^IInventory inventory slot-number slot-x slot-y]
  (let [slot (Slot. inventory (int slot-number) (int slot-x) (int slot-y))]
    (set! (.-slotNumber slot) (.size ^List (.-inventorySlots container)))
    (.add ^List (.-inventorySlots container) slot)
    (.add ^List (.-inventoryItemStacks container) nil)
    slot))

(defn add-slots [container inventory slots]
  (let [add-slot-to-this (partial add-slot-to-container container inventory)]
    (loop [slots slots]
      (if (not (first slots))
        true
        (let [slot (first slots)]
          (add-slot-to-this (first slot) (second slot) (nth slot 2))
          (recur (rest slots)))))))

(defn add-player-hotbar [container player-inventory]
  (let [hotbar-slots (mapv #(vector %1 (+ (* %1 18) 8) 142) (range 9))]
    (add-slots container player-inventory hotbar-slots)))

(defn add-player-inventory [container player-inventory]
  (let [make-slot (fn [x y]
                    [(+ x (* y 9) 9) (+ 8 (* x 18)) (+ 84 (* y 18))])
        slots (into [] (for [x (range 9)
                             y (range 3)]
                         (make-slot x y)))]
    (add-slots container player-inventory slots)))

(defmacro defcontainer [name-ns class-name & args]
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

(defn open-gui [^EntityPlayer player mod-instance id world x y z]
  (.openGui player mod-instance id world x y z))
