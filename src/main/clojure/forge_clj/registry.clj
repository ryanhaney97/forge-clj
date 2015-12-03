(ns forge-clj.registry
  "Contains all of the functions used to register things with Minecraft Forge."
  (:import
   [net.minecraft.block Block]
   [net.minecraft.item Item]
   [net.minecraftforge.common IExtendedEntityProperties]
   [net.minecraft.entity Entity]
   [net.minecraftforge.common MinecraftForge]
   [cpw.mods.fml.common.registry GameRegistry]
   [cpw.mods.fml.common FMLCommonHandler IWorldGenerator]
   [cpw.mods.fml.common.network NetworkRegistry]))

(defmulti register
  "Multimethod consisting of a series of register functions. Handles the following arguments:
  (block-object, name): registers block with specified name.
  (item-object, name): registers item with specified name.
  (block-object, name, blockitem): registers block with specified name and blockitem object.
  (world-generator): registers a world generator.
  (world-generator, priority): registers a world generator with the specified priority."
  (fn [element & args] [(count args) (type element)]))
(defmethod register [1 Block] [element forge-name]
  (GameRegistry/registerBlock element forge-name)
  element)
(defmethod register [1 Item] [element forge-name]
  (GameRegistry/registerItem element forge-name)
  element)
(defmethod register [2 Block] [element forge-name blockitem]
  (GameRegistry/registerBlock element nil forge-name (object-array []))
  (GameRegistry/registerItem blockitem forge-name)
  element)
(defmethod register [0 IWorldGenerator] [generator]
  (register generator 0))
(defmethod register [1 IWorldGenerator] [generator mod-priority]
  (GameRegistry/registerWorldGenerator generator mod-priority))

(defn register-tile-entity
  "Registers a Tile Entity with the specified id"
  [entity id]
  (GameRegistry/registerTileEntity entity id))

(defn register-events
  "Registers an Event Handler."
  [handler]
  (.register (.bus (FMLCommonHandler/instance)) handler)
  (.register MinecraftForge/EVENT_BUS handler))

(defn register-extended-properties
  "Takes an Entity and an id and registers extended entity properties on it."
  [^Entity entity id ^IExtendedEntityProperties properties]
  (.registerExtendedProperties entity (str id) properties))

(defn register-gui-handler
  "Registers a gui handler for the specified mod instance."
  [mod-instance handler]
  (.registerGuiHandler ^NetworkRegistry (NetworkRegistry/INSTANCE) mod-instance handler))
