(ns forge-clj.ui
  (:require [forge-clj.core :refer [genobj]])
  (:import
   [cpw.mods.fml.common.network IGuiHandler NetworkRegistry]
   [net.minecraft.inventory Container Slot]
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

(defn add-slot-to-container [^Container container inventory slot-number slot-x slot-y]
  (let [slot (Slot. inventory slot-number slot-x slot-y)]
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

(defn merge-item-stack [^Container this ^ItemStack istack start-index end-index reverse?]
  (let [indexes (range start-index end-index)
        indexes (if reverse? (reverse indexes) indexes)
        per-slot (fn [index]
                   (let [^Slot slot (.get ^List (.-inventorySlots this) index)
                         ^ItemStack localstack (.getStack slot)]
                     (if (and localstack
                              (= (.getItem localstack) (.getItem istack))
                              (or (not (.getHasSubtypes istack)) (= (.getItemDamage istack) (.getItemDamage localstack)))
                              (ItemStack/areItemStackTagsEqual istack localstack))
                       (let [total-size (+ (.-stackSize istack) (.-stackSize localstack))]
                         (if (<= total-size (.getMaxStackSize istack))
                           (do
                             (set! (.-stackSize localstack) 0)
                             (set! (.-stackSize istack) total-size)
                             (.onSlotChanged slot)
                             true)
                           (if (< (.-stackSize istack) (.getMaxStackSize istack))
                             (do
                               (set! (.-stackSize localstack) (- (.-stackSize localstack) (- (.getMaxStackSize istack) (.-stackSize istack))))
                               (set! (.-stackSize istack) (.getMaxStackSize istack))
                               (.onSlotChanged slot)
                               true)
                             false)))
                       false)))
        per-slot-second (fn [[index & others]]
                          (if (and index (or (and (not reverse?) (< index end-index)) (and reverse? (>= index start-index))))
                            (let [^Slot slot (.get ^List (.-inventorySlots this) index)
                                  ^ItemStack localstack (.getStack slot)]
                              (if (= localstack nil)
                                (do
                                  (.putStack slot ^ItemStack (.copy istack))
                                  (.onSlotChanged slot)
                                  (set! (.-stackSize istack) 0)
                                  true)
                                (recur others)))
                            false))
        slot-calls (doall (map per-slot indexes))
        slot-calls-other (per-slot-second indexes)]
    (or (not (empty? (filter identity slot-calls))) slot-calls-other)))

(defmacro make-container [player-inventory bound-inventory & args]
  (let [objdata (apply hash-map args)
        hotbar? (:include-hotbar objdata)
        inventory? (:include-inventory objdata)
        slots (:slots objdata)
        objdata (if (get-in objdata [:override :merge-item-stack])
                  objdata
                  (assoc-in objdata [:override :merge-item-stack] merge-item-stack))
        objdata (dissoc objdata :slots :include-hotbar :include-inventory)]
    `(doto
       (genobj Container [] ~objdata)
       ~(if slots
          `(add-slots ~bound-inventory ~slots)
          `(identity))
       ~(if inventory?
          `(add-player-inventory ~player-inventory)
          `(identity))
       ~(if hotbar?
          `(add-player-hotbar ~player-inventory)
          `(identity)))))

(defn open-gui [^EntityPlayer player mod-instance id world x y z]
  (.openGui player mod-instance id world x y z))
