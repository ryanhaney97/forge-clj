(ns forge-clj.registry
  "Contains all of the functions used to register things with Minecraft Forge."
  (:import
   [net.minecraft.block Block]
   [net.minecraft.item Item]
   [net.minecraft.world.biome BiomeGenBase]
   [net.minecraftforge.common IExtendedEntityProperties]
   [net.minecraft.entity Entity]
   [net.minecraftforge.common MinecraftForge BiomeDictionary BiomeManager BiomeDictionary$Type BiomeManager$BiomeType BiomeManager$BiomeEntry]
   [cpw.mods.fml.common.registry GameRegistry]
   [cpw.mods.fml.common FMLCommonHandler IWorldGenerator]
   [cpw.mods.fml.common.network NetworkRegistry]))

(def biome-type-list
  {:desert BiomeManager$BiomeType/DESERT
   :warm BiomeManager$BiomeType/WARM
   :cool BiomeManager$BiomeType/COOL
   :icy BiomeManager$BiomeType/ICY})

(def biome-group-list
  {:hot BiomeDictionary$Type/HOT
   :cold BiomeDictionary$Type/COLD
   :sparse BiomeDictionary$Type/SPARSE
   :dense BiomeDictionary$Type/DENSE
   :wet BiomeDictionary$Type/WET
   :dry BiomeDictionary$Type/DRY
   :savanna BiomeDictionary$Type/SAVANNA
   :coniferous BiomeDictionary$Type/CONIFEROUS
   :jungle BiomeDictionary$Type/JUNGLE
   :spooky BiomeDictionary$Type/SPOOKY
   :dead BiomeDictionary$Type/DEAD
   :lush BiomeDictionary$Type/LUSH
   :nether BiomeDictionary$Type/NETHER
   :end BiomeDictionary$Type/END
   :mushroom BiomeDictionary$Type/MUSHROOM
   :magical BiomeDictionary$Type/MAGICAL
   :ocean BiomeDictionary$Type/OCEAN
   :river BiomeDictionary$Type/RIVER
   :water BiomeDictionary$Type/WATER
   :mesa BiomeDictionary$Type/MESA
   :forest BiomeDictionary$Type/FOREST
   :plains BiomeDictionary$Type/PLAINS
   :mountain BiomeDictionary$Type/MOUNTAIN
   :hills BiomeDictionary$Type/HILLS
   :swamp BiomeDictionary$Type/SWAMP
   :sandy BiomeDictionary$Type/SANDY
   :snowy BiomeDictionary$Type/SNOWY
   :wasteland BiomeDictionary$Type/WASTELAND
   :beach BiomeDictionary$Type/BEACH
   :desert BiomeDictionary$Type/DESERT
   :frozen BiomeDictionary$Type/FROZEN})

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
(defmethod register [3 BiomeGenBase] [^BiomeGenBase biome biome-types biome-groups spawn-weight]
  (if (vector? biome-types)
    (doall (map #(BiomeManager/addBiome (get biome-type-list %1 %1) (BiomeManager$BiomeEntry. biome (int spawn-weight))) biome-types))
    (BiomeManager/addBiome (get biome-type-list biome-types biome-types) (BiomeManager$BiomeEntry. biome (int spawn-weight))))
  (BiomeManager/addSpawnBiome biome)
  (BiomeDictionary/registerBiomeType biome (into-array BiomeDictionary$Type (map #(get biome-group-list %1 %1) biome-groups))))

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
