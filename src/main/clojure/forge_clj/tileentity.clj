(ns forge-clj.tileentity
  (:require
   [forge-clj.nbt :refer [read-tag-data! write-tag-data! map->nbt nbt->map]]
   [forge-clj.core :refer [defassocclass gen-classname]]
   [clojure.string :as string])
  (:import
   [net.minecraft.nbt NBTTagCompound]
   [net.minecraft.tileentity TileEntity]
   [net.minecraft.network.play.server S35PacketUpdateTileEntity]
   [net.minecraft.world World]))

;Creates a Tile Entity class.
(defmacro deftileentity [name-ns class-name & args]
  (let [classdata (apply hash-map args)
        prefix (str class-name "-")
        classdata (assoc-in classdata [:expose 'readFromNBT] 'superReadFromNBT)
        classdata (assoc-in classdata [:expose 'writeToNBT] 'superWriteToNBT)
        fullname (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname class-name)))
        this-sym (with-meta 'this {:tag fullname})
        sync-data (get classdata :sync-data [])
        on-load (get classdata :on-load `(constantly nil))
        on-save (get classdata :on-save `(constantly nil))
        classdata (dissoc classdata :on-load :on-save :sync-data)]
    `(do
       (defassocclass TileEntity ~name-ns ~class-name ~classdata)
       (defn ~(symbol (str prefix "readFromNBT")) [~'this ~'compound]
         (~'.superReadFromNBT ~this-sym ~'compound)
         (read-tag-data! (~'.-data ~this-sym) ~'compound)
         (~on-load ~this-sym))
       (defn ~(symbol (str prefix "writeToNBT")) [~'this ~'compound]
         (~'.superWriteToNBT ~this-sym ~'compound)
         (~on-save ~this-sym)
         (write-tag-data! (~'.-data ~this-sym) ~'compound))
       (defn ~(symbol (str prefix "getDescriptionPacket")) [~'this]
         (S35PacketUpdateTileEntity. (.-xCoord ~this-sym) (.-yCoord ~this-sym) (.-zCoord ~this-sym) 1 (map->nbt (select-keys (deref (~'.-data ~this-sym)) ~sync-data) (NBTTagCompound.))))
       (defn ~(symbol (str prefix "onDataPacket")) [~'this ~'network-manager ~(with-meta 'packet `{:tag S35PacketUpdateTileEntity})]
         (swap! (~'.-data ~this-sym) merge (nbt->map (.func_148857_g ~'packet)))))))

;Gets the tile entity in the world at the specified coordinates. For convenience.
(defn get-tile-entity-at [^World world x y z]
  (.getTileEntity world (int x) (int y) (int z)))
