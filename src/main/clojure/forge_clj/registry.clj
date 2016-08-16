(ns forge-clj.registry
  "Contains all of the functions used to register things with Minecraft Forge."
  (:require
    [clojure.string :as string]
    [forge-clj.util])
  (:import
    [net.minecraft.block Block]
    [net.minecraft.item Item]
    [net.minecraft.world.biome BiomeGenBase]
    [net.minecraftforge.common IExtendedEntityProperties]
    [net.minecraft.entity Entity]
    [net.minecraft.tileentity TileEntity]
    [net.minecraftforge.common MinecraftForge BiomeDictionary BiomeManager BiomeDictionary$Type BiomeManager$BiomeType BiomeManager$BiomeEntry]
    [net.minecraftforge.fml.common.registry GameRegistry EntityRegistry]
    [net.minecraftforge.fml.common FMLCommonHandler IWorldGenerator]
    [net.minecraftforge.fml.common.network NetworkRegistry IGuiHandler]))

(def biome-type-list
  {:desert BiomeManager$BiomeType/DESERT
   :warm   BiomeManager$BiomeType/WARM
   :cool   BiomeManager$BiomeType/COOL
   :icy    BiomeManager$BiomeType/ICY})

(def biome-group-list
  {:hot        BiomeDictionary$Type/HOT
   :cold       BiomeDictionary$Type/COLD
   :sparse     BiomeDictionary$Type/SPARSE
   :dense      BiomeDictionary$Type/DENSE
   :wet        BiomeDictionary$Type/WET
   :dry        BiomeDictionary$Type/DRY
   :savanna    BiomeDictionary$Type/SAVANNA
   :coniferous BiomeDictionary$Type/CONIFEROUS
   :jungle     BiomeDictionary$Type/JUNGLE
   :spooky     BiomeDictionary$Type/SPOOKY
   :dead       BiomeDictionary$Type/DEAD
   :lush       BiomeDictionary$Type/LUSH
   :nether     BiomeDictionary$Type/NETHER
   :end        BiomeDictionary$Type/END
   :mushroom   BiomeDictionary$Type/MUSHROOM
   :magical    BiomeDictionary$Type/MAGICAL
   :ocean      BiomeDictionary$Type/OCEAN
   :river      BiomeDictionary$Type/RIVER
   :water      BiomeDictionary$Type/WATER
   :mesa       BiomeDictionary$Type/MESA
   :forest     BiomeDictionary$Type/FOREST
   :plains     BiomeDictionary$Type/PLAINS
   :mountain   BiomeDictionary$Type/MOUNTAIN
   :hills      BiomeDictionary$Type/HILLS
   :swamp      BiomeDictionary$Type/SWAMP
   :sandy      BiomeDictionary$Type/SANDY
   :snowy      BiomeDictionary$Type/SNOWY
   :wasteland  BiomeDictionary$Type/WASTELAND
   :beach      BiomeDictionary$Type/BEACH
   :desert     BiomeDictionary$Type/DESERT
   :frozen     BiomeDictionary$Type/FROZEN})

(defn register-block
  ([^Block block forge-name]
   (GameRegistry/registerBlock block (str forge-name))
   block)
  ([^Block block forge-name blockitem]
   (GameRegistry/registerBlock block nil forge-name (object-array []))
   (GameRegistry/registerItem blockitem forge-name)
   block))

(defn register-item [^Item item forge-name]
  (GameRegistry/registerItem item forge-name)
  item)

(defn register-generator
  ([generator]
   (register-generator generator 0))
  ([^IWorldGenerator generator mod-priority]
   (GameRegistry/registerWorldGenerator generator mod-priority)
   generator))

(defn register-biome [^BiomeGenBase biome biome-types biome-groups spawn-weight]
  (if (vector? biome-types)
    (doall (map #(BiomeManager/addBiome (get biome-type-list %1 %1) (BiomeManager$BiomeEntry. biome (int spawn-weight))) biome-types))
    (BiomeManager/addBiome (get biome-type-list biome-types biome-types) (BiomeManager$BiomeEntry. biome (int spawn-weight))))
  (BiomeManager/addSpawnBiome biome)
  (BiomeDictionary/registerBiomeType biome (into-array BiomeDictionary$Type (map #(get biome-group-list %1 %1) biome-groups)))
  biome)

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

(defn register-entity
  ([entity entity-name id mod tracking-range update-frequency sends-velocity-updates?]
    (EntityRegistry/registerModEntity entity entity-name id mod tracking-range update-frequency sends-velocity-updates?))
  ([entity entity-name id mod tracking-range update-frequency sends-velocity-updates? egg-color1 egg-color2]
   (EntityRegistry/registerModEntity entity entity-name id mod tracking-range update-frequency sends-velocity-updates? egg-color1 egg-color2)))

(defn register-gui-handler
  "Registers a gui handler for the specified mod instance."
  [mod-instance handler]
  (.registerGuiHandler ^NetworkRegistry (NetworkRegistry/INSTANCE) mod-instance handler))

(defn differentiate-class-registers [& args]
  (let [superclasses (supers ^Class (first args))]
    (if (and (= (count args) 2) (some #{TileEntity} superclasses))
      (apply register-tile-entity args)
      (if (and (or (= (count args) 7) (= (count args) 9)) (some #{Entity} superclasses))
        (apply register-entity args)
        (if (= (count args) 1)
          (register-events (first args)))))))

(defn differentiate-entity-registers [& args]
  (if (and (= (count args) 3) (instance? IExtendedEntityProperties (last args)))
    (apply register-extended-properties args)))

(defn differentiate-other-registers [& args]
  (if (and (= (count args) 4) (instance? BiomeGenBase (first args)))
    (apply register-biome args)
    (if (instance? Entity (first args))
      (apply differentiate-entity-registers args)
      (if (and (= (count args) 2) (instance? IGuiHandler (second args)))
        (apply register-gui-handler args)
        (if (instance? Class (first args))
          (apply differentiate-class-registers args)
          (if (= (count args) 1)
            (register-events (first args))))))))

(defmulti register
          "Multimethod consisting of a series of register functions. Handles the following arguments:
          (block-object, name): registers block with specified name.
          (item-object, name): registers item with specified name.
          (block-object, name, blockitem): registers block with specified name and blockitem object.
          (world-generator): registers a world generator.
          (world-generator, priority): registers a world generator with the specified priority."
          (fn [element & args] [(count args) (type element)]))
(defmethod register [1 Block] [block forge-name]
  (register-block block forge-name))
(defmethod register [1 Item] [item forge-name]
  (register-item item forge-name))
(defmethod register [2 Block] [element forge-name blockitem]
  (register-block element forge-name blockitem))
(defmethod register [0 IWorldGenerator] [generator]
  (register-generator generator))
(defmethod register [1 IWorldGenerator] [generator mod-priority]
  (register-generator generator mod-priority))
(defmethod register :default [& args]
  (apply differentiate-other-registers args))