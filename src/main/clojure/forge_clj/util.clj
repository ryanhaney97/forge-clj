(ns forge-clj.util
  (:require
   [clojure.string :as string]
   [clojure.set :as cset]
   [forge-clj.core :refer [defobj]])
  (:import
   [java.util Random]
   [net.minecraft.block Block]
   [net.minecraft.item Item ItemStack]
   [net.minecraft.creativetab CreativeTabs]
   [net.minecraft.entity.player EntityPlayer]
   [net.minecraft.entity.item EntityItem]
   [net.minecraft.util ChatComponentText Vec3 MovingObjectPosition]
   [net.minecraft.inventory IInventory]
   [net.minecraft.server MinecraftServer]
   [net.minecraft.world World]
   [cpw.mods.fml.common.registry GameRegistry]))

(defn server-worlds []
  (.-worldServers ^MinecraftServer (MinecraftServer/getServer)))

(defmulti make-itemstack (fn [item amount metadata] (type item)))
(defmethod make-itemstack Item [^Item item amount metadata]
  (ItemStack. item (int amount) (int metadata)))
(defmethod make-itemstack Block [^Block block amount metadata]
  (ItemStack. block (int amount) (int metadata)))

;Makes an ItemStack. For convenience.
(defn itemstack
  ([item]
   (itemstack item 1))
  ([item amount]
   (itemstack item amount 0))
  ([item amount metadata]
   (make-itemstack item amount metadata)))

;Convenient .isRemote check function. Looks cleaner.
(defn remote? [^World world]
  (.isRemote world))

;Utility function. Given a map and a function, applies that function to all values in the map.
(defn update-map-vals [func m]
  (into {} (map #(vector (key %1) (func (val %1))) m)))

;Utility function. Given a map and a function, applies that function to all keys in the map.
(defn update-map-keys [func m]
  (cset/map-invert (update-map-vals func (cset/map-invert m))))

;Extremely basic function that returns the absolute value of a number. For convenience.
(defn abs [n]
  (if (< n 0)
    (* -1 n)
    n))

(defmacro defmemo [memo-name arg-vector & args]
  `(def ~memo-name
     (memoize
      (fn ~arg-vector
        ~@args))))

(defmacro deftab [tab-name & args]
  (let [obj-data (apply hash-map args)]
    `(defobj CreativeTabs [~(str tab-name)] ~tab-name ~obj-data)))

(defn get-item
  ([name-and-id]
   (let [split-id (string/split name-and-id #":")]
     (get-item (first split-id) (second split-id))))
  ([modid item-name]
   (GameRegistry/findItem (str modid) (str item-name))))

(defn get-block
  ([name-and-id]
   (let [split-id (string/split name-and-id #":")]
     (get-block (first split-id) (second split-id))))
  ([modid block-name]
   (GameRegistry/findBlock (str modid) (str block-name))))

(defn printchat [^EntityPlayer player s]
  (.addChatComponentMessage player (ChatComponentText. (str s))))

(defn drop-items [^World world x y z]
  (let [tile-entity (.getTileEntity world (int x) (int y) (int z))]
    (if (instance? IInventory tile-entity)
      (let [tile-entity ^IInventory tile-entity
            per-stack (fn [^ItemStack istack]
                        (if (and istack (< 0 (.-stackSize istack)))
                          (let [rand-x (+ (* (rand) 0.8) 0.1)
                                rand-y (+ (* (rand) 0.8) 0.1)
                                rand-z (+ (* (rand) 0.8) 0.1)
                                entity-item (EntityItem. world (+ x rand-x) (+ y rand-y) (+ z rand-z) (itemstack (.getItem istack) (.-stackSize istack) (.getItemDamage istack)))]
                            (if (.hasTagCompound istack)
                              (.setTagCompound (.getEntityItem entity-item) (.copy (.getTagCompound istack))))
                            (set! (.-motionX entity-item) (* (rand) 0.05))
                            (set! (.-motionY entity-item) (+ (* (rand) 0.05) 0.2))
                            (set! (.-motionZ entity-item) (* (rand) 0.05))
                            (.spawnEntityInWorld world entity-item)
                            (set! (.-stackSize istack) 0))))]
        (doall (map #(per-stack (.getStackInSlot tile-entity %1)) (range (.getSizeInventory tile-entity))))))))

(defn get-look-coords [^EntityPlayer player ^World world]
  (let [^Vec3 pos-vec (Vec3/createVectorHelper (.-posX player) (+ (.-posY player) (.getEyeHeight player)) (.-posZ player))
        ^Vec3 look-vec (.getLookVec player)
        ^MovingObjectPosition mop (.rayTraceBlocks world pos-vec look-vec)]
    [(.-blockX mop) (.-blockY mop) (.-blockZ mop) (.-sideHit mop)]))

(defn construct [klass & args]
  (clojure.lang.Reflector/invokeConstructor klass (into-array Object args)))
